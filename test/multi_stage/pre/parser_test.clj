(ns multi-stage.pre.parser-test
  (:require [multi-stage.pre.parser :refer :all]
            [multi-stage.pre.ast :refer :all]
            [multi-stage.common.core :refer [mock-gensyms]]
            [multi-stage.test-utils :refer [verify-pattern remove-gensym with-generated-vars]]
            [clojure.test :as t :refer [testing deftest]]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument
 (stest/enumerate-namespace
  ['meliae.patterns
   'multi-stage.common.core
   'multi-stage.pre.ast
   'multi-stage.pre.parser]))

(defmacro parse [sexp]
  (mock-gensyms (clj->pre (remove-gensym sexp) {})))

(defmacro verify-parse [expected input]
  `(let [expected# (mock-gensyms ~expected)]
     (verify-pattern (parse ~input) expected#)))

(deftest constant-tests
  (testing "string"
    (verify-parse
     (->literal unknown-source-context "hello")
     "hello"))
  (testing "keyword"
    (verify-parse
     (->literal unknown-source-context :foo)
     :foo))
  (testing "character"
    (verify-parse
     (->literal unknown-source-context \c)
     \c))
  (testing "single values"
    (testing "true"
      (verify-parse
       (->literal unknown-source-context true)
       true)
      (verify-parse
       (->literal unknown-source-context true)
       true)
      (verify-parse
       (->literal unknown-source-context nil)
       nil)
      (verify-parse
       (->literal unknown-source-context (list))
       ()))))

(deftest literal-collections-test
  (testing "vector"
    (verify-parse
     (->literal-vector unknown-source-context
                       [(->literal unknown-source-context :a)
                        (->literal unknown-source-context :b)
                        (->literal-vector unknown-source-context
                                          [(->literal unknown-source-context :c)
                                           (->literal unknown-source-context :d)])])
     [:a :b [:c :d]]))
  (testing "set"
    (verify-parse
     (->literal-set unknown-source-context
                    [(->literal unknown-source-context :b)
                     (->literal unknown-source-context :a)])
     #{:a :b}))
  (testing "map"
    (verify-parse
     (->literal-map unknown-source-context
                    [[(->literal unknown-source-context :a) (->literal unknown-source-context 1)]
                     [(->literal unknown-source-context :b) (->literal unknown-source-context 2)]])
     {:a 1, :b 2})))

(deftest function-call-tests
  (testing "plus"
    (testing "(+ 1 2)"
      (verify-parse
       (->apply unknown-source-context
                (->symbol-reference unknown-source-context `+)
                [(->literal unknown-source-context 1)
                 (->literal unknown-source-context 2)])
       (+ 1 2)))))

(deftest let-tests
  (testing "Simple let"
    (verify-parse
     (with-generated-vars [x-var x unknown-source-context]
       (->let unknown-source-context
              x-var
              (->literal unknown-source-context 1)
              (->apply unknown-source-context
                       (->symbol-reference unknown-source-context `+)
                       [(->variable-reference unknown-source-context x-var)
                        (->variable-reference unknown-source-context x-var)])))
     (let [x 1]
       (+ x x))))
  (testing "Empty let should be discarded"
    (verify-parse
     (->apply unknown-source-context
              (->symbol-reference unknown-source-context `+)
              [(->literal unknown-source-context 1)
               (->literal unknown-source-context 2)])
     (let []
       (+ 1 2))))
  (testing "let with two dependent bindings"
    (verify-parse
     (with-generated-vars [x-var x unknown-source-context
                           y-var y unknown-source-context]
       (->let unknown-source-context
              x-var
              (->literal unknown-source-context 1)
              (->let unknown-source-context
                     y-var
                     (->variable-reference unknown-source-context x-var)
                     (->apply unknown-source-context
                              (->symbol-reference unknown-source-context `+)
                              [(->variable-reference unknown-source-context x-var)
                               (->variable-reference unknown-source-context y-var)]))))
     (let [x 1
           y x]
       (+ x y))))
  (testing "shadowing let"
    (verify-parse
     (with-generated-vars [x1-var x unknown-source-context
                           x2-var x unknown-source-context]
       (->let unknown-source-context
              x1-var
              (->literal unknown-source-context 1)
              (->let unknown-source-context
                     x2-var
                     (->literal unknown-source-context 2)
                     (->variable-reference unknown-source-context x2-var))))
     (let [x 1
           x 2]
       x)))
  (testing "overwrite + with * via let"
    (verify-parse
     (with-generated-vars [plus-var + unknown-source-context
                           mul-var mul unknown-source-context
                           x-var x unknown-source-context
                           y-var y unknown-source-context]
       (->let unknown-source-context
              plus-var
              (->fn unknown-source-context
                    mul-var
                    [x-var y-var]
                    (->apply unknown-source-context
                             (->symbol-reference unknown-source-context `*)
                             [(->variable-reference unknown-source-context x-var)
                              (->variable-reference unknown-source-context y-var)]))
              (->apply unknown-source-context
                       (->variable-reference unknown-source-context plus-var)
                       [(->literal unknown-source-context 5)
                        (->literal unknown-source-context 2)])))
     (let [+ (fn mul [x y]
               (* x y))]
       (+ 5 2)))))

(deftest fn-test
  (testing "one argument"
    (verify-parse
     (with-generated-vars [f-var f unknown-source-context
                           x-var x unknown-source-context]
       (->fn unknown-source-context
             f-var
             [x-var]
             (->apply unknown-source-context
                      (->symbol-reference unknown-source-context `+)
                      [(->variable-reference unknown-source-context x-var)
                       (->variable-reference unknown-source-context x-var)])))
     (fn f [x]
       (+ x x))))
  (testing "two nested lambdas"
    (verify-parse
     (with-generated-vars [outer-var outer unknown-source-context
                           x-var x unknown-source-context
                           inner-var inner unknown-source-context
                           y-var y unknown-source-context]
       (->fn unknown-source-context
             outer-var
             [x-var]
             (->fn unknown-source-context
                   inner-var
                   [y-var]
                   (->apply unknown-source-context
                            (->symbol-reference unknown-source-context `+)
                            [(->variable-reference unknown-source-context x-var)
                             (->apply unknown-source-context
                                      (->symbol-reference unknown-source-context `*)
                                      [(->variable-reference unknown-source-context y-var)
                                       (->variable-reference unknown-source-context y-var)])]))))
     (fn outer [x]
       (fn inner [y]
         (+ x (* y y))))))
  (testing "fn without parameters"
    (verify-parse
     (with-generated-vars [f-var f unknown-source-context]
       (->fn unknown-source-context
             f-var
             []
             (->literal unknown-source-context 1)))
     (fn f []
       1)))
  (testing "two arguments"
    (verify-parse
     (with-generated-vars [f-var f unknown-source-context
                           x-var x unknown-source-context
                           y-var y unknown-source-context]
       (->fn unknown-source-context
             f-var
             [x-var y-var]
             (->apply unknown-source-context
                      (->symbol-reference unknown-source-context `+)
                      [(->variable-reference unknown-source-context x-var)
                       (->variable-reference unknown-source-context y-var)])))
     (fn f [x y]
       (+ x y))))
  (testing "fn* via reader macro"
    ;; This uses the syntax (fn* [x] (+ x 1))
    ;; (without the parenthesis which would be used for multiple arities)
    (verify-parse
     (with-generated-vars [fn-var MOCKED-fn0 unknown-source-context
                           p1-var p1 unknown-source-context]
       (->fn unknown-source-context
             fn-var
             [p1-var]
             (->apply unknown-source-context
                      (->symbol-reference unknown-source-context `+)
                      [(->variable-reference unknown-source-context p1-var)
                       (->literal unknown-source-context 1)])))
     #(+ % 1))))

(deftest apply-tests
  (testing "1 argument"
    (verify-parse
     (with-generated-vars [f-var f unknown-source-context
                           x-var x unknown-source-context]
       (->apply unknown-source-context
                (->fn unknown-source-context
                      f-var
                      [x-var]
                      (->apply unknown-source-context
                               (->symbol-reference unknown-source-context `+)
                               [(->variable-reference unknown-source-context x-var)
                                (->literal unknown-source-context 1)]))
                [(->literal unknown-source-context 5)]))
     ((fn f [x]
        (+ x 1))
      5)))
  (testing "2 arguments"
    (verify-parse
     (with-generated-vars [f-var f unknown-source-context
                           x-var x unknown-source-context
                           y-var y unknown-source-context]
       (->apply unknown-source-context
                (->fn unknown-source-context
                      f-var
                      [x-var y-var]
                      (->apply unknown-source-context
                               (->symbol-reference unknown-source-context `+)
                               [(->variable-reference unknown-source-context x-var)
                                (->variable-reference unknown-source-context y-var)]))
                [(->literal unknown-source-context 3)
                 (->literal unknown-source-context 4)]))
     ((fn f [x y]
        (+ x y))
      3 4)))
  (testing "recursive apply fn"
    (verify-parse
     (with-generated-vars [f-var f unknown-source-context
                           x-var x unknown-source-context]
       (->apply unknown-source-context
                (->fn unknown-source-context
                      f-var
                      [x-var]
                      (->apply unknown-source-context
                               (->variable-reference unknown-source-context f-var)
                               [(->variable-reference unknown-source-context x-var)]))
                [(->literal unknown-source-context 5)]))
     ((fn f [x]
        (f x))
      5))))

(deftest if-tests
  (testing "2 branches"
    (verify-parse
     (->if unknown-source-context
           (->literal unknown-source-context 1)
           (->literal unknown-source-context 2)
           (->literal unknown-source-context 3))
     (if 1 2 3)))
  (testing "1 branch (second defaults to nil)"
    (verify-parse
     (->if unknown-source-context
           (->literal unknown-source-context 1)
           (->literal unknown-source-context 2)
           (->literal unknown-source-context nil))
     (if 1 2)))
  (testing "when (macro-expansion)"
    (verify-parse
     (->if unknown-source-context
           (->literal unknown-source-context 1)
           (->literal unknown-source-context 2)
           (->literal unknown-source-context nil))
     (when 1 2))))

(deftest loop-recur-test
  (testing "one argument"
    (verify-parse
     (with-generated-vars [loop-var MOCKED-loop0 unknown-source-context
                           x1-var x unknown-source-context
                           x2-var x unknown-source-context]
       (->let unknown-source-context
              x2-var
              (->literal unknown-source-context 1)
              (->apply unknown-source-context
                       (->fn unknown-source-context
                             loop-var
                             [x1-var]
                             (->apply unknown-source-context
                                      (->variable-reference unknown-source-context loop-var)
                                      [(->apply unknown-source-context
                                                (->symbol-reference unknown-source-context `inc)
                                                [(->variable-reference unknown-source-context x1-var)])]))
                       [(->variable-reference unknown-source-context x2-var)])))
     (loop [x 1]
       (recur (inc x))))))

;; TODO test implicit-do conversion
;; TODO test lift-loop
;; TODO two arguments
;; TODO test recur target shadowing
