(ns multi-stage.post.ast-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [multi-stage.post.ast :refer :all]))

(stest/instrument
 (stest/enumerate-namespace
  '[multi-stage.post.ast]))

(deftest test-dfs-order
  (testing "(+ x (a b) y)"
    (is (= (->invoke (smart-variable `+)
                     [(smart-variable 'x)
                      (->invoke (smart-variable `+)
                                [(smart-variable 'a)
                                 (smart-variable 'b)]
                                false
                                ['a 'b `+])
                      (smart-variable 'y)]
                     true
                     ['x 'a 'b `+ 'y `+])
           (smart-invoke (smart-variable `+)
                         [(smart-variable 'x)
                          (smart-invoke (smart-variable `+)
                                        [(smart-variable 'a)
                                         (smart-variable 'b)])
                          (smart-variable 'y)])))))

(deftest test-smart-do
  (testing "empty bodies"
    (is (= (smart-literal nil)
           (smart-do []))))
  (testing "one body"
    (is (= (smart-variable 'a)
           (smart-do [(smart-variable 'a)]))))
  (testing "two bodies"
    (is (= (->do [(smart-variable 'a)
                  (smart-variable 'b)]
                 '[a b])
           (smart-do [(smart-variable 'a) (smart-variable 'b)]))))
  (testing "three bodies"
    (is (= (->do [(smart-variable 'a)
                  (smart-variable 'b)
                  (smart-variable 'c)]
                 '[a b c ])
           (smart-do [(smart-variable 'a) (smart-do [(smart-variable 'b) (smart-variable 'c)])])))
    (is (= (->do [(smart-variable 'a)
                  (smart-variable 'b)
                  (smart-variable 'c)]
                 '[a b c])
           (smart-do [(smart-do [(smart-variable 'a) (smart-variable 'b)]) (smart-variable 'c)])))))

(deftest test-smart-let*
  (testing "no arguments"
    (is (= (smart-literal 1)
           (smart-let* [] [(smart-literal 1)]))))
  (testing "simple 1-arg case"
    (is (= (->let* [['a (smart-literal 1)]]
             [(smart-literal 2)]
             [])
           (smart-let* [['a (smart-literal 1)]]
             [(smart-literal 2)]))))
  (testing "implicit do, 1 binding"
    (is (= (->let* [['a (smart-literal 1)]]
             [(smart-literal 2)
              (smart-literal 3)]
             [])
           (smart-let* [['a (smart-literal 1)]]
             [(smart-do [(smart-literal 2)
                         (smart-literal 3)])]))))
  (testing "(let [a 1] a)"
    (is (= (smart-literal 1)
           (smart-let* [['a (smart-literal 1)]]
             [(smart-variable 'a)]))))

  (testing "(let [x 42 a 1] a)"
    (is (= (->let* [['x (smart-literal 42)]]
             [(smart-literal 1)]
             [])
           (smart-let* [['x (smart-literal 42)]
                        ['a (smart-literal 1)]]
             [(smart-variable 'a)]))))
  (testing "(let [x 42 a 1] (if a 1 2))"
    (is (= (->let* [['x (smart-literal 42)]]
             [(smart-if (smart-literal 43)
                        (smart-literal 1)
                        (smart-literal 2))]
             [])
           (smart-let* [['x (smart-literal 42)]
                        ['a (smart-literal 43)]]
             [(smart-if (smart-variable 'a)
                        (smart-literal 1)
                        (smart-literal 2))]))))
  (comment
    (testing "undo let-insertion"
      (is (= (->invoke (smart-variable `+)
                       [(smart-literal 4)
                        (smart-literal 5)]
                       false
                       `[+])
             (smart-let* [['x (smart-literal 4)]
                          ['y (smart-literal 5)]]
               [(smart-invoke (smart-variable `+)
                              [(smart-variable 'x)
                               (smart-variable 'y)])])))
      ))
  )

(deftest tail-call-elimination
  (testing "no loop"
    (is (= (smart-loop 'loop
                       [['a (smart-literal 1)]
                        ['b (smart-literal 2)]]
                       [(smart-invoke (smart-variable `+)
                                      [(smart-variable 'a)
                                       (smart-variable 'b)])])
           (smart-invoke (smart-fn* `the-loop
                                    {:args ['a 'b]
                                     :bodies [(smart-invoke (smart-variable `+)
                                                            [(smart-variable 'a)
                                                             (smart-variable 'b)])]})
                         [(smart-literal 1)
                          (smart-literal 2)]))))
  (testing "simple loop-recur"
    (is (= (smart-loop 'loop
                       [['x (smart-literal 1)]
                        ['y (smart-literal 2)]]
                       [(->recur [(smart-invoke (smart-variable `inc)
                                                [(smart-variable 'x)
                                                 (smart-variable 'y)])])])
           (smart-invoke (smart-fn* `the-loop
                                    {:args ['a 'b]
                                     :bodies [(smart-invoke (smart-variable `the-loop)
                                                            [(smart-invoke (smart-variable `inc)
                                                                           [(smart-variable 'x)])
                                                             (smart-invoke (smart-variable `dec)
                                                                           [(smart-variable 'y)])])]})
                         [(smart-literal 1)
                          (smart-literal 2)])))))

(meliae.patterns/print-pattern
 (smart-invoke (smart-fn* `the-loop
                          {:args ['a 'b]
                           :bodies [(smart-invoke (smart-variable `the-loop)
                                                  [(smart-invoke (smart-variable `inc)
                                                                 [(smart-variable 'x)])
                                                   (smart-invoke (smart-variable `dec)
                                                                 [(smart-variable 'y)])])]})
               [(smart-literal 1)
                (smart-literal 2)]))

;; TODO more tests
