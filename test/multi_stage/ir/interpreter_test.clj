(ns multi-stage.ir.interpreter-test
  (:refer-clojure :exclude [reify])
  (:require [multi-stage.ir.interpreter :refer :all]
            [multi-stage.ir.ast :refer :all]
            [multi-stage.ir.value :refer :all]
            [clojure.test :refer :all]
            [meliae.patterns :refer [print-pattern]]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument
 '[multi-stage.ir.interpreter
   multi-stage.ir.ast
   multi-stage.ir.value])

(defn debug [x]
  (print "debug: ")
  (print-pattern x)
  (println)
  x)

(deftest basic-tests
  (testing "function inlining and constant folding"
    (is (= (->constant 3)
           (debug (evalmsg []
                           (->apply (->fn 1
                                          (->let (->literal 1)
                                                 (->let (->literal 2)
                                                        (->variable 1)
                                                        't2)
                                                 't1)
                                          'f
                                          ['x])
                                    [(->literal 3)])
                           nil)))))
  (testing "run factorial 4"
    (is (= (->constant 24)
           (run #(evalms []
                         (->apply (->fn 1
                                        (->if (->apply (->primitive-symbol `>)
                                                       [(->variable 1)
                                                        (->literal 0)])
                                              (->apply (->primitive-symbol `*)
                                                       [(->variable 1)
                                                        (->apply (->variable 0)
                                                                 [(->apply (->primitive-symbol `-)
                                                                           [(->variable 1)
                                                                            (->literal 1)])])])
                                              (->literal 1))
                                        'fac
                                        ['x])
                                  [(->literal 4)])
                         nil))))))

(deftest symbol-inlining
  (testing "let"
    (is (= (->constant 3)
           (evalmsg []
                    (->let (->primitive-symbol `+)
                           (->apply (->variable 0)
                                    [(->literal 1)
                                     (->literal 2)])
                           'x)
                    nil))))
  (testing "apply-fn"
    (is (= (->constant 3)
           (evalmsg []
                    (->let (->fn 1
                                 (->apply (->variable 1)
                                          [(->literal 1)
                                           (->literal 2)])
                                 'f
                                 ['x])
                           (->apply (->variable 0)
                                    [(->primitive-symbol `+)])
                           'x)
                    nil)))))


(deftest keyword-execution
  (testing "let"
    (is (= (->constant 42)
           (evalmsg []
                    (->let (->literal :foo)
                           (->apply (->variable 0)
                                    [(->literal {:foo 42})])
                           'kw)
                    nil)))))

(deftest special-functions
  (testing "set"
    (is (= (->constant :b)
           (evalmsg []
                    (->apply (->literal #{:a :b}) [(->literal :b)])
                    nil)))
    (is (= (->constant nil)
           (evalmsg []
                    (->apply (->literal #{:a :b}) [(->literal :x)])
                    nil))))
  (testing "map"
    (is (= (->constant 42)
           (evalmsg []
                    (->apply (->literal {:a 42})
                             [(->literal :a) (->literal :not-found)])
                    nil)))
    (is (= (->constant :not-found)
           (evalmsg []
                    (->apply (->literal {:a 42})
                             [(->literal :x) (->literal :not-found)])
                    nil))))
  (testing "vector"
    (is (= (->constant :bar)
           (evalmsg []
                    (->apply (->literal [:foo :bar])
                             [(->literal 1)])
                    nil)))))
