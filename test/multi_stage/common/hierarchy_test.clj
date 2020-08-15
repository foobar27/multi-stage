(ns multi-stage.common.hierarchy-test
  (:require [multi-stage.common.hierarchy :as h]
            [clojure.test :refer [deftest testing are is]]))

(def tree-hierarchy
  (-> (h/make-hierarchy)
      (h/derive :b :a)
      (h/derive :c :b)
      (h/derive :d :c)
      (h/derive :e :c)))

(def diamond-hierarchy
  (-> (h/make-hierarchy)
      (h/derive ::mammal ::animal)
      (h/derive ::bird ::animal)
      (h/derive ::griffin ::mammal)
      (h/derive ::griffin ::bird)))

(deftest using-hierarchies
  (testing "tree hierarchy"
    (testing "parent relationships"
      (are [x xs]
          (= xs (h/parents tree-hierarchy x))
        :a nil
        :b #{:a}
        :c #{:b}
        :d #{:c}
        :e #{:c}))
    (testing "ancestor relationships"
      (are [x xs]
          (= xs (h/ancestors tree-hierarchy x))
        :a nil
        :b #{:a}
        :c #{:a :b}
        :d #{:a :b :c}
        :e #{:a :b :c}))
    (testing "descendants relationships"
      (are [x xs]
          (= xs (h/descendants tree-hierarchy x))
        :a #{:b :c :d :e}
        :b #{   :c :d :e}
        :c #{      :d :e}
        :d nil
        :e nil))
    (testing "isa?"
      (are [child parent b]
          (is (= b (boolean (h/isa? tree-hierarchy child parent))))
        :x :x true
        :b :a true
        :a :b false
        :d :a true)))
  (testing "diamond hierarchy"
    (is (h/isa? diamond-hierarchy ::griffin ::animal))
    (is (h/isa? diamond-hierarchy ::griffin ::bird)))
  (testing "vector hierarchy"
    (is (h/isa? diamond-hierarchy  [::griffin ::animal] [::animal ::animal]))
    (is (not (h/isa? diamond-hierarchy [::animal ::bird] [::griffin ::bird])))
    (is (not (h/isa? diamond-hierarchy [::animal ::foo] [::animal ::bar])))))

(deftest broken-hierarchies
  (testing "a tag cannot be its own parent"
    (is (thrown-with-msg? Throwable #"\(not= child parent\)"
                          (h/derive tree-hierarchy :a :a))))
  (testing "a tag cannot be its own ancestor"
    (is (thrown-with-msg? Throwable #":d already has :a as an ancestor"
                          (h/derive tree-hierarchy :d :a))))
  (testing "cyclic derivation"
    (is (thrown-with-msg? Throwable #"Cyclic derivation: :d has :a as an ancestor."
                          (h/derive tree-hierarchy :a :d)))))
