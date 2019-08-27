(ns multi-stage.ir.integration-test
  (:require [multi-stage.ir.core :refer [lift run]]
            [multi-stage.test-utils :refer [verify-pattern remove-gensym]]
            [meliae.patterns :refer [print-pattern]]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [multi-stage.test-utils :refer [specialize]]))

(stest/instrument (stest/enumerate-namespace ['meliae.patterns 'multi-stage.ir.ast 'multi-stage.clojure.ast]))

;; TODO copy & paste

(def start-ab-simple
  (specialize
   (fn matches [r]
     (fn matches-inner [s]
       (if (seq r)
         (if (seq s)
           (if (= (lift (first r))
                  (first s))
             ((matches (rest r)) (rest s))))
         (lift true))))
   ["ab"]))

(def start-ab-optimized
  (specialize
   (let [matches-generic (fn matches-generic [maybe-lift]
                           (fn rec [r]
                             (fn [s]
                               (if (seq r)
                                 (if (seq s)
                                   (if (= (lift (first r))
                                          (first s))
                                     ((rec (rest r)) (rest s))
                                     (maybe-lift false))
                                   (maybe-lift false))
                                 (maybe-lift true)))))
         matches-spec (matches-generic (fn [e] (lift e)))
         matches-gen (matches-generic (fn [e] e))]
     (fn [r]
       (if (< (count r) 20)
         (run 0 (lift (matches-spec r)))
         (matches-gen r))))
   ["ab"]))

(deftest start-ab-test
  (are [input result]
      (and (= (boolean result) (boolean (start-ab-simple input)))
           (= (boolean result) (boolean (start-ab-optimized input))))
    "" false
    "a" false
    "ab" true
    "abc" true
    "acb" false))

(comment
  (specialize
   (let [maybe-lift (fn [e] e)]
     (let [star-loop (fn star-loop [match-pattern]
                       (fn [c]
                         (maybe-lift (fn inner-loop [string]
                                       (if (= (maybe-lift :yes) (match-pattern string))
                                         (maybe-lift :yes)
                                         (let [[s & string] string]
                                           (if (= (maybe-lift :done) s)
                                             (maybe-lift :no)
                                             (if (= '_ c)
                                               (inner-loop string)
                                               (if (= (maybe-lift c) s)
                                                 (inner-loop string)
                                                 (maybe-lift :no))))))))))]
       (let [match-here (fn match-here [[p & pattern]]
                          (fn [string]
                            (if (= :done p)
                              (maybe-lift :yes)
                              (let [m (fn [[s & string]]
                                        (if (= '_ p)
                                          (if (= (maybe-lift :done) s)
                                            (maybe-lift :no)
                                            ((match-here pattern) string))
                                          (if (= (maybe-lift :done) s)
                                            (maybe-lift :no)
                                            (if (= (maybe-lift p) s)
                                              ((match-here pattern) string)
                                              (maybe-lift :no)))))]
                                (let [[p & pattern] pattern]
                                  (if (= :done p)
                                    (m string)
                                    (if (= '* p)
                                      (((star-loop (match-here pattern))
                                        p)
                                       string)
                                      (m string))))))))]
         (let [match (fn match [pattern]
                       (if (= :done (first pattern))
                         (maybe-lift (fn [string]
                                       (maybe-lift :yes)))
                         (maybe-lift (match-here pattern))))]
           match))))
   ['(_ a _ * :done)]))


(comment
  (def matcher-example01
    `((let [maybe-lift (fn [e] e)]
        ~@matcher-src)
      '(_ a  _ * :done)
      '(b a :done)))

  (evalms (list `(let maybe-lift (lambda _ e e) ,matcher-src)
                `(_ * a _ * done)
                `(b a done))
          `((((,pink-eval-exp3 (var 0))
              nil-env)
             (var 1))
            (var 2))))

