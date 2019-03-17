(ns towers.clojure.ast-test
  (:require [clojure.test :refer :all]
            [towers.clojure.ast :refer :all]))

(deftest test-smart-do
  (testing "empty bodies"
    (is (->literal nil)
        (smart-do [])))
  (testing "one body"
    (is (= (->variable 'a)
           (smart-do [(->variable 'a)]))))
  (testing "two bodies"
    (is (= (->do [(->variable 'a)
                  (->variable 'b)])
           (smart-do [(->variable 'a) (->variable 'b)]))))
  (testing "three bodies"
    (is (= (->do [(->variable 'a)
                  (->variable 'b)
                  (->variable 'c)])
           (smart-do [(->variable 'a) (smart-do [(->variable 'b) (->variable 'c)])])))
    (is (= (->do [(->variable 'a)
                  (->variable 'b)
                  (->variable 'c)])
           (smart-do [(smart-do [(->variable 'a) (->variable 'b)]) (->variable 'c)])))))

(deftest test-smart-let*
  (testing "no arguments"
    (is (= (->literal 1)
           (smart-let* [] [(->literal 1)]))))
  (testing "simple 1-arg case"
    (is (= (->let* [['a (->literal 1)]]
             [(->literal 2)])
           (smart-let* [['a (->literal 1)]]
             [(->literal 2)]))))
  (testing "implicit do, 1 binding"
    (is (= (->let* [['a (->literal 1)]]
             [(->literal 2)
              (->literal 3)])
           (smart-let* [['a (->literal 1)]]
             [(smart-do [(->literal 2)
                         (->literal 3)])]))))
  (testing "(let [a 1] a)"
    (is (= (->literal 1)
           (smart-let* [['a (->literal 1)]]
             [(->variable 'a)]))))

  (testing "(let [x 42 a 1] a)"
    (is (= (->let* [['x (->literal 42)]]
             [(->literal 1)])
           (smart-let* [['x (->literal 42)]
                        ['a (->literal 1)]]
             [(->variable 'a)]))))
  (testing "(let [x 42 a 1] (if a 1 2))"
    (is (= (->let* [['x (->literal 42)]]
             [(->if (->literal 43)
                    (->literal 1)
                    (->literal 2))])
           (smart-let* [['x (->literal 42)]
                        ['a (->literal 43)]]
             [(smart-if (->variable 'a)
                        (->literal 1)
                        (->literal 2))]))))
  (testing "undo let-insertion"
    (is (= (->invoke `+
                     [(->literal 4)
                      (->literal 5)])
           (smart-let* [['x (->literal 4)]
                        ['y (->literal 5)]]
             [(->invoke `+
                        [(->variable 'x)
                         (->variable 'y)])])))
    )
  )

;; TODO more tests
