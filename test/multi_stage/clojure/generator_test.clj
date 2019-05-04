(ns multi-stage.clojure.generator-test
  (:require [clojure.test :refer :all]
            [multi-stage.clojure.ast :refer :all]
            [multi-stage.clojure.generator :refer :all]))

(deftest literal-collections-test
  (testing "vector"
    (is (= [:a :b]
           (generate (smart-vector [(smart-literal :a)
                                    (smart-literal :b)])))))
  (testing "set"
    (is (= #{:a :b}
           (generate (smart-set [(smart-literal :a)
                                 (smart-literal :b)])))))
  (testing "map"
    (is (= {:a 1 :b 2}
           (generate (smart-map [[(smart-literal :a) (smart-literal 1)]
                                 [(smart-literal :b) (smart-literal 2)]]))))))
