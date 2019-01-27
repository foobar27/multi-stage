(ns towers.base.integration-test
  (:require [meliae.patterns :refer [print-pattern]]
            [towers.base.ast :refer :all]
            [towers.base.parser :refer :all]
            [towers.base.interpreter :refer [evalmsg lift]]
            [clojure.test :as t]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def ^:private arithmetic-expression-generator
  (gen/recursive-gen (fn [inner-gen]
                       (gen/fmap (fn [xs] (apply list xs))
                                 (gen/tuple (gen/elements #{'+ '*}) inner-gen inner-gen)))
                     gen/int))

(defspec randomly-nested-arithmetic-expressions 100
  (prop/for-all [expression arithmetic-expression-generator]
                (= (->constant (eval expression))
                   (evalmsg [] (clj->expression expression)))))


(def ^:private xyz-expression-generator
  (gen/fmap (fn [[expression & args]]
              (apply list
                     (concat '(fn [x y z])
                             [expression])
                     args))
            (apply gen/tuple
                   (gen/recursive-gen (fn [inner-gen]
                                        (gen/fmap (fn [xs] (apply list xs))
                                                  (gen/tuple (gen/elements #{'+ '*}) inner-gen inner-gen)))
                                      (gen/one-of [gen/int
                                                   (gen/elements #{'x 'y 'z})]))
                   (map (fn [i] gen/int) (range 3)))))

(defspec randomly-nested-xyz-expressions 100
  (prop/for-all [expression xyz-expression-generator]
                (= (->constant (eval expression))
                   (evalmsg [] (clj->expression expression)))))

(def ^:private factorial-code
  '(fn self [n]
     (if (> n 0)
       (* n (self (- n 1)))
       1))) 

(defn- factorial-code-application [n]
  (list factorial-code n))

(deftest factorial-evaluation
  (doseq [n (range 10)]
    (let [expression (factorial-code-application n)]
      (is (= (->constant (eval expression))
             (evalmsg [] (clj->expression (factorial-code-application n))))))))

(defn matches-clj [r s]
  (if (empty? r)
    true
    (if (empty? s)
      false
      (if (= (first r)
             (first s))
        (matches-clj (rest r)
                     (rest s))
        false))))

(deftest matches-clj-tests
  (are [expected r s] (= expected (boolean (matches-clj (seq r) (seq s))))
    true "Hell" "Hello"
    true "Hello" "Hello"
    true "H" "Hello"
    true "" "Hello"
    false "h" "Hello"
    false "Hellow" "Hello"))

(def matches
  (parse
   (fn matches [r s]
     (if (empty? r)
       (lift 1) ;; TODO true
       (if (empty? s)
         (lift 0)
         (if (= (lift (first r))
                (first s))
           (matches (rest r)
                    (rest s))
           (lift 0)))))))


(defn matches-ab [x]
  ;; TODO need to run the interpreter
  ;; TODO re-write into clojure-like syntax (needs to lookup the definition of "matches" somewhere)
  (->run (->literal 0) (->lift (->apply matches '(a b)))))

(print-pattern (matches-ab [\a \b]))

(evalmsg #{}
         (->run (->lambda (->literal 0)) (->literal 42)))

(evalmsg #{}
         (->run (->literal 0) (->lift (->apply matches (->literal ('a 'b))))))

(print-pattern (->run 0 (->lift (->apply matches '(a b)))))

(->run 0
       (->lift (->apply (->lambda (->lambda (->if (->empty? (->variable 1 r)) (->lift (->literal 1)) (->if (->empty? (->variable 3 s)) (->lift (->literal 0)) (->if (->equals? (->lift (->car (->variable 1 r))) (->car (->variable 3 s))) (->apply (->apply (->variable 0 matches) (->cdr (->variable 1 r))) (->cdr (->variable 3 s))) (->lift (->literal 0))))))) (a b))))


;; (print-pattern (matches-ab '(a b c)))

;; (->run 0
;;        (->lift (->apply (->lambda (->lambda
;;                                    (->if (->empty? (->variable 1 r))
;;                                          (->lift (->literal 1))
;;                                          (->if (->empty? (->variable 3 s))
;;                                                (->lift (->literal 0))
;;                                                (->if (->equals?
;;                                                       (->lift (->car (->variable 1 r)))
;;                                                       (->car (->variable 3 s)))
;;                                                      (->apply (->apply (->variable 0 matches)
;;                                                                        (->cdr (->variable 1 r)))
;;                                                               (->cdr (->variable 3 s)))
;;                                                      (->lift (->literal 0)))))))
;;                         (a b))))
