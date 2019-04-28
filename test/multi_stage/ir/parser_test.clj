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

(deftest primitive-call-test
  (testing "plus"
    (testing "(+ 1 2)"
      (verify-parse
       (->primitive-call `+ [(->literal 1) (->literal 2)])
       (+ 1 2)))))

(deftest let-test
  (testing "Simple let"
    (verify-parse
     (let [x (->variable 0 "x")]
       (->let (->literal 1)
              (->primitive-call `+ [x x])))
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
            (->let (->variable 0 "x")
                   (->primitive-call `+ [(->variable 0 "x")
                                         (->variable 1 "y")])))
     (let [x 1
           y x]
       (+ x y)))))

(deftest fn-test
  (testing "one  argument"
    (verify-parse
     (let [x (->variable 1 "x")]
       (->lambda (->primitive-call `+ [x
                                       (->primitive-call `* [x x])])))
     (fn [x]
       (+ x (* x x)))))
  (testing "two nested lambdas"
    (verify-parse
     (let [x (->variable 1 "x")
           y (->variable 3 "y")]
       (->lambda (->lambda (->primitive-call `+ [x
                                                 (->primitive-call `* [y y])]))))
     (fn [x]
       (fn [y]
         (+ x (* y y))))))
  (testing "fn without argument"
    (verify-parse
     (->lambda (->literal 1))
     (fn []
       1)))
  (testing "two arguments"
    (verify-parse
     (->lambda (->lambda (->primitive-call `+ [(->variable 1 "x")
                                               (->variable 3 "y")])))
     (fn [x y]
       (+ x y))))
  (testing "fn* via reader macro"
    ;; This usess the syntax (fn* [x] (+ x 1))
    ;; (without the parenthesis which would be used for multiple arities)
    (verify-parse
     (->lambda (->primitive-call `+ [(->variable 1 "p1")
                                     (->literal 1)]))
     #(+ % 1))))

(deftest apply-test
  (testing "1 argument"
    (verify-parse
     (->apply (->lambda (->primitive-call `+ [(->literal 1)
                                              (->variable 1 "x")]))
              [(->literal 5)])
     ((fn [x] (+ 1 x))
      5)))
  (testing "2 arguments"
    (verify-parse
     (->apply (->apply (->lambda (->lambda (->primitive-call `+ [(->variable 1 "x")
                                                                 (->variable 3 "y")])))
                       [(->literal 3)])
              [(->literal 4)])
     ((fn [x y] (+ x y))
      3 4)))
  (testing "recursive apply fn"
    (verify-parse
     (->apply (->lambda (->apply (->variable 0 "f")
                                 [(->variable 1 "x")]))
              [(->literal 5)])
     ((fn f [x]
        (f x))
      5))))

(deftest conditions-test
  (testing "3 arguments"
    (verify-parse
     (->if (->literal 3)
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
     (->apply (->lambda (->apply (->variable 0 "loop")
                                 [(->primitive-call `inc [(->variable 1 "x")])]))
              [(->literal 1)])
     (loop [x 1]
       (recur (inc x)))))
  (testing "shadowing"
    (verify-parse
     (->let (->literal 1)
            (->apply (->lambda (->apply (->variable 1 "loop")
                                        [(->primitive-call `inc [(->variable 2 "x")])]))
                     [(->variable 0 "x")]))
     (let [x 1]
       (loop [x x]
         (recur (inc x)))))))
;; TODO two arguments
;; TODO test recur target shadowing


(->apply (->lambda (->apply (->variable 0 loop19401) [(->primitive-call clojure.core/inc ((->variable 1 x)))])) [(->literal 1)])

(meliae.patterns/print-pattern (parse (loop [x 1] (recur (inc x)))))
