(ns multi-stage.common.multi-methods-test
  (:require [multi-stage.common.hierarchy :as h]
            [multi-stage.common.multi-methods :as mm]
            [clojure.test :refer [deftest testing are is]]))

(deftest basic-multi-method-test
  (let [too-simple (-> (mm/make-multi-method 'too-simple
                                             identity
                                             (atom (h/make-hierarchy))
                                             :default)
                       (mm/add-method ::a ::a-fn)
                       (mm/add-method ::b ::b-fn)
                       (mm/add-method :default ::default-fn))]
    (testing "Check basic dispatch"
      (are [dispatch-val the-fn]
          (= the-fn (mm/get-method too-simple dispatch-val))
        ::a ::a-fn
        ::b ::b-fn))
    (testing "Remove a method"
      (is (= ::default-fn (mm/get-method too-simple ::unknown))))))

(deftest isa?-multi-method-test
  (let [hierarchy (-> (h/make-hierarchy)
                      (h/derive ::rectangle ::shape)
                      (h/derive ::square ::rectangle)
                      (h/derive ::conic-section ::shape)
                      (h/derive ::ellipse ::conic-section)
                      (h/derive ::circle ::ellipse)
                      (h/derive ::parabola ::conic-section))
        shapes (-> (mm/make-multi-method 'shapes
                                         identity
                                         (atom hierarchy)
                                         :default)
                   (mm/add-method ::rectangle ::rectangle-fn)
                   (mm/add-method ::ellipse ::ellipse-fn)
                   (mm/add-method :default ::default-fn))]
    (are [dispatch-val the-fn]
        (= the-fn (mm/get-method shapes dispatch-val))
      ::rectangle     ::rectangle-fn
      ::square        ::rectangle-fn
      ::ellipse       ::ellipse-fn
      ::circle        ::ellipse-fn
      ::conic-section ::default-fn
      ::parabola      ::default-fn)))

(deftest preferences-multi-method-test
  (let [hierarchy (-> (h/make-hierarchy)
                      (h/derive ::rectangle ::shape))
        bar (-> (mm/make-multi-method 'bar
                                      identity
                                      (atom hierarchy)
                                      :default)
                (mm/add-method [::rectangle ::shape] ::rectangle-shape-fn)
                (mm/add-method [::shape ::rectangle] ::shape-rectangle-fn))]
    (testing "In case of a conflict, an exception should be thrown"
      (is (thrown? java.lang.IllegalArgumentException
                   (mm/get-method bar [::rectangle ::rectangle]))))
    (testing "The prefers method returns empty table"
      (is (= {} (mm/prefers bar))))
    (let [bar (mm/prefer-method bar [::rectangle ::shape] [::shape ::rectangle])]
      (testing "Adding a preference to resolve it dispatches correctly"
        (is (= ::rectangle-shape-fn (mm/get-method bar [::rectangle ::rectangle])))))))
