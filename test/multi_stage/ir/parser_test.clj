(ns multi-stage.ir.parser_test
  (:require [multi-stage.common.core :refer [mock-gensyms]]

            [multi-stage.pre.parser :refer [clj->pre generate-variable!]]

            [multi-stage.ir.core :refer [lift run]]
            [multi-stage.ir.ast :as ir-ast :refer :all]
            [multi-stage.ir.parser :as ir-parser :refer [pre->ir]]
            
            ;;[multi-stage.clojure.generator :as clj-gen]

            [multi-stage.test-utils :refer [verify-pattern remove-gensym with-generated-vars]]
            [meliae.patterns :refer [print-pattern]]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument
 (stest/enumerate-namespace
  ['meliae.patterns
   'multi-stage.common.core
   'multi-stage.pre.ast
   'multi-stage.pre.parser
   'multi-stage.ir.ast
   'multi-stage.ir.parser
   ]))

(defmacro parse [sexp]
  (mock-gensyms
   (pre->ir
    (clj->pre (remove-gensym sexp)
              {})
    {})))

(defmacro verify-parse [expected input]
  `(let [expected# (mock-gensyms ~expected)]
     (verify-pattern (parse ~input) expected#)))

(deftest constant-tests
  (testing "string"
    (verify-parse
     (->literal "hello")
     "hello")))

(deftest literal-collections-test
  (testing "map"
    (verify-parse
     (->literal-map [[(->literal :a) (->literal 1)]
                     [(->literal :b) (->literal 2)]])
     {:a 1, :b 2})))

(deftest function-call-test
  (testing "clojure function call"
    (verify-parse
     (->apply (->primitive-symbol `+)
              [(->literal 1) (->literal 2)])
     (+ 1 2))))
;; TODO other kinds of symbol references

(deftest let-tests
  (testing "Simple let"
    (verify-parse
     (->let (->literal 1)
            (->apply (->primitive-symbol `+)
                     [(->variable 0) (->variable 0)])
            'x)
     (let [x 1]
       (+ x x))))
  (testing "Shadowing let"
    (verify-parse
     (->let (->literal 1)
            (->let (->literal 2)
                   (->variable 1)
                   'x)
            'x)
     (let [x 1
           x 2]
       x))))

(deftest fn-test
  (testing "one argument"
    (verify-parse
     (->lambda 1
               (->apply (->primitive-symbol `+)
                        [(->variable 1)
                         (->variable 1)])
               'f
               '[x])
     (fn f [x]
       (+ x x))))
  (testing "two nested fns"
    (verify-parse
     (->lambda 1
               (->lambda 1
                         (->apply (->primitive-symbol `+)
                                  [(->variable 1)
                                   (->variable 3)])
                         'inner
                         '[y])
               'outer
               '[x])
     (fn outer [x]
       (fn inner [y]
         (+ x y)))))
  (testing "two arguments"
    (verify-parse
     (->lambda 2
               (->apply (->primitive-symbol `+)
                        [(->variable 1)
                         (->variable 2)])
               'f
               '[x y])
     (fn f [x y]
       (+ x y))))
  (testing "0 arguments"
    (verify-parse
     (->lambda 0
               (->literal 1)
               'f
               [])
     (fn f []
       1))))

(deftest apply-test
  (testing "1 argument"
    (verify-parse
     (->apply (->lambda 1
                        (->apply (->primitive-symbol `+)
                                 [(->variable 1)
                                  (->literal 1)])
                        'f
                        '[x])
              [(->literal 5)])
     ((fn f [x]
        (+ x 1))
      5)))
  (testing "2 arguments"
    (verify-parse
     (->apply (->lambda 2
                        (->apply (->primitive-symbol `+)
                                 [(->variable 1)
                                  (->variable 2)])
                        'f
                        '[x y])
              [(->literal 2)
               (->literal 3)])
     ((fn f [x y]
        (+ x y))
      2 3)))
  (testing "recursive apply fn"
    (verify-parse
     (->apply (->lambda 1
                        (->apply (->variable 0)
                                 [(->variable 1)])
                        'f
                        '[x])
              [(->literal 5)])
     ((fn f [x]
        (f x))
      5))))

(deftest if-test
  (testing "1 branch"
    (verify-parse
     (->if (->literal 1)
           (->literal 2)
           (->literal nil))
     (if 1 2))))
