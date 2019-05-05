(ns multi-stage.ir.parser_test
  (:require [multi-stage.clojure.generator :as clj-gen]
            [multi-stage.ir.ast :as ir-ast :refer :all]
            [multi-stage.ir.parser :as ir-parser :refer [parse]]
            [multi-stage.ir.core :refer [lift run]]
            [multi-stage.test-utils :refer [verify-pattern remove-auto-gensym]]
            [meliae.patterns :refer [print-pattern]]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument (stest/enumerate-namespace ['meliae.patterns 'multi-stage.ir.ast 'multi-stage.clojure.ast]))

(defmacro verify-parse [expected input]
  `(let [expected# ~expected]
     (verify-pattern (parse ~input) expected#)))

(deftest constants
  (testing "string"
    (verify-parse
     (->literal "hello")
     "hello"))
  (testing "keyword"
    (verify-parse
     (->literal :foo)
     :foo))
  (testing "character"
    (verify-parse
     (->literal \c)
     \c))
  (testing "single values"
    (testing "true"
      (verify-parse
       (->literal true)
       true))
    (testing "false"
      (verify-parse
       (->literal false)
       false))
    (testing "nil"
      (verify-parse
       (->literal nil)
       nil))
    (testing "empty list"
      (verify-parse
       (->literal '())
       ()))))

(deftest primitive-call-test
  (testing "plus"
    (testing "(+ 1 2)"
      (verify-parse
       (->primitive-call `+ [(->literal 1) (->literal 2)])
       (+ 1 2)))))

(deftest let-test
  (testing "Simple let"
    (verify-parse
     (let [x (->variable 0)]
       (->let (->literal 1)
              (->primitive-call `+ [x x])
              'x))
     (let [x 1]
       (+ x x))))
  (testing "Empty let should be discarded"
    (verify-parse
     (->primitive-call `+ [(->literal 1) (->literal 2)])
     (let []
       (+ 1 2))))
  (testing "let with two dependent bindings"
    (verify-parse
     (->let (->literal 1)
            (->let (->variable 0)
                   (->primitive-call `+ [(->variable 0)
                                         (->variable 1)])
                   'y)
            'x)
     (let [x 1
           y x]
       (+ x y))))
  (testing "shadowing let"
    (verify-parse
     (->let (->literal 1)
            (->let (->literal 2)
                   (->variable 1)
                   'x)
            'x)
     (let [x 1
           x 2]
       x)))
  (testing "override + with * via let"
    (verify-parse
     (->let (->lambda 2
                      (->primitive-call `* [(->variable 1) (->variable 2)])
                      'mul
                      '[x y])
            (->apply (->variable 0)
                     [(->literal 5) (->literal 2)])
            '+)
     (let [+ (fn mul [x y] (* x y))]
       (+ 5 2)))))

(deftest fn-test
  (testing "one  argument"
    (verify-parse
     (let [x (->variable 1)]
       (->lambda 1
                 (->primitive-call `+ [x
                                       (->primitive-call `* [x x])])
                 'f
                 '[x]))
     (fn f [x]
       (+ x (* x x)))))
  (testing "two nested lambdas"
    (verify-parse
     (let [x (->variable 1)
           y (->variable 3)]
       (->lambda 1
                 (->lambda 1
                           (->primitive-call `+ [x
                                                 (->primitive-call `* [y y])])
                           
                           'inner
                           '[y])
                 'outer
                 '[x]))
     (fn outer [x]
       (fn inner [y]
         (+ x (* y y))))))
  (testing "fn without argument"
    (verify-parse
     (->lambda 0
               (->literal 1)
               'f
               [])
     (fn f[]
       1)))
  (testing "two arguments"
    (verify-parse
     (->lambda 2
               (->primitive-call `+ [(->variable 1)
                                     (->variable 2)])
               'f
               '[x y])
     (fn f [x y]
       (+ x y))))
  (testing "fn* via reader macro"
    ;; This usess the syntax (fn* [x] (+ x 1))
    ;; (without the parenthesis which would be used for multiple arities)
    (verify-parse
     (->lambda 1
               (->primitive-call `+ [(->variable 1)
                                     (->literal 1)])
               'unnamed
               '[p1])
     #(+ % 1))))

(deftest apply-test
  (testing "1 argument"
    (verify-parse
     (->apply (->lambda 1
                        (->primitive-call `+ [(->literal 1)
                                              (->variable 1)])
                        'f
                        '[x])
              [(->literal 5)])
     ((fn f [x] (+ 1 x))
      5)))
  (testing "2 arguments"
    (verify-parse
     (->apply (->lambda 2
                        (->primitive-call `+ [(->variable 1)
                                              (->variable 2)])
                        'f
                        '[x y])
              [(->literal 3) (->literal 4)])
     ((fn f [x y] (+ x y))
      3 4)))
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

(deftest conditions-test
  (testing "3 arguments"
    (verify-parse
     (->if (->literal 1)
           (->literal 2)
           (->literal 3))
     (if 1 2 3)))
  (testing "2 argument (default nil)"
    (verify-parse
     (->if (->literal 1)
           (->literal 2)
           (->literal nil))
     (if 1 2))))

(deftest loop-recur-test
  (testing "one argument"
    (verify-parse
     (->apply (->lambda 1
                        (->apply (->variable 0)
                                 [(->primitive-call `inc [(->variable 1)])])
                        'loop
                        '[x])
              [(->literal 1)])
     (loop [x 1]
       (recur (inc x)))))
  (testing "shadowing"
    (verify-parse
     (->let (->literal 1)
            (->apply (->lambda 1
                               (->apply (->variable 1)
                                        [(->primitive-call `inc [(->variable 2)])])
                               'loop
                               '[x])
                     [(->variable 0)])
            'unnamed-let)
     (let [x 1]
       (loop [x x]
         (recur (inc x)))))))
;; TODO two arguments
;; TODO test recur target shadowing

(deftest literal-collections-test
  (testing "vector"
    (verify-parse
     (->literal-vector [(->literal :a)
                        (->literal :b)
                        (->literal-vector [(->literal :c)
                                           (->literal :d)])])
     [:a :b [:c :d]]))
  (testing "set"
    (verify-parse
     (->literal-set [(->literal :b)
                     (->literal :a)])
     #{:a :b}))
  (testing "map"
    (verify-parse
     (->literal-map [[(->literal :a) (->literal 1)]
                     [(->literal :b) (->literal 2)]])
     {:a 1 :b 2})))
