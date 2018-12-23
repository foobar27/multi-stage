(ns towers.base.integration-test
  (:require [meliae.patterns :refer [print-pattern]]
            [towers.base.ast :refer :all]
            [towers.base.parser :refer :all]
            [towers.base.interpreter :refer [evalmsg]]
            [clojure.test :as t]
            [clojure.test :refer :all]))

(deftest arithmetic-expression
  (is (= (->constant 7)
         (evalmsg [] (parse (+ 1 (* 2 3)))))))

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
  (->run 0 (->lift (->apply matches '(a b)))))

(evalmsg #{['a 'b 'c]}
         (->run 0 (->lift (->apply matches '(a b)))))

(print-pattern (matches-ab '(a b c)))

(->run 0
       (->lift (->apply (->lambda (->lambda
                                   (->if (->empty? (->variable 1 r))
                                         (->lift (->literal 1))
                                         (->if (->empty? (->variable 3 s))
                                               (->lift (->literal 0))
                                               (->if (->equals?
                                                      (->lift (->car (->variable 1 r)))
                                                      (->car (->variable 3 s)))
                                                     (->apply (->apply (->variable 0 matches)
                                                                       (->cdr (->variable 1 r)))
                                                              (->cdr (->variable 3 s)))
                                                     (->lift (->literal 0)))))))
                        (a b))))
