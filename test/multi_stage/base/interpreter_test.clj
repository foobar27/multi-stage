(ns multi-stage.base.interpreter-test
  (:refer-clojure :exclude [reify eval])
  (:require [clojure.test :refer :all]
            [meliae.patterns :refer [print-pattern]]
            [multi-stage.base.ast :refer :all]
            [multi-stage.base.interpreter :refer :all]
            [clojure.spec.test.alpha :as stest]))

;; (stest/instrument (stest/enumerate-namespace ['meliae.patterns 'multi-stage.base.ast 'multi-stage.base.interpreter]))

;; Inspired by https://github.com/namin/pink/blob/master/base-tests.scm

(defn debug [x]
  (print "debug: ")
  (print-pattern x)
  (println)
  x)

(deftest basic-tests
  (testing "apply fn let let identity"
    (let [p (->apply (->lambda
                      (->let (->literal 1)
                             (->let (->literal 2)
                                    (->variable 1 ""))))
                     (->literal 3))]
      (testing "evalmsg"
        (is (= (debug (evalmsg [] p))
               (->constant 3)))))))

(deftest running-tests
  (testing "factorial 4"
    (is (= (run #(evalms [] (->apply (->lambda
                                      (->if (->gt (->variable 1 "") (->literal 0))
                                            (->times (->variable 1 "")
                                                     (->apply (->variable 0 "")
                                                              (->minus (->variable 1 "")
                                                                       (->literal 1))))
                                            (->literal 1)))
                                     (->literal 4))))
           (->constant 24))))
  (testing "number?-1"
    (is (= (run #(evalms [] (->literal? (->literal 1))))
           (->literal 1)))))
;; TODO eq?-+

(def base-fac-anf
  (->let (->lambda
          (->let (->gt (->variable 1 "fresh") 0)
                 (->let (->if (->variable 2 "fresh")
                              (->let (->minus (->variable 1 "fresh")
                                              (->literal 1))
                                     (->let (->apply (->variable 0 "fresh")
                                                     (->variable 3 "fresh"))
                                            (->let (->times (->variable 1 "fresh")
                                                            (->variable 4 "fresh"))
                                                   (->variable 5 "fresh"))))
                              (->literal 1))
                        (->variable 3 "fresh"))))
         (->variable 0 "fresh")))


(deftest reification-tests 
  (testing "reifyc-1"
    (is (= (reifyc #(evalms []
                            (->if (->lift (->literal 0))
                                  (->plus (->lift (->literal 1))
                                          (->lift (->literal 2)))
                                  (->lift (->literal 0)))))
           (->let
            (->if (->literal 0)
                  (->let (->plus (->literal 1) (->literal 2))
                         (->variable 0 "fresh"))
                  (->literal 0))
            (->variable 0 "fresh")))))
  (testing "reify (+ (* e1 e2) (* e3 e4))"
    (is (= (debug (reifyc #(evalms []
                                   (->plus (->times (->plus (->lift (->literal 0))
                                                            (->lift (->literal 0)))
                                                    (->plus (->lift (->literal 1))
                                                            (->lift (->literal 1))))
                                           (->times (->plus (->lift (->literal 2))
                                                            (->lift (->literal 2)))
                                                    (->plus (->lift (->literal 3))
                                                            (->lift (->literal 3))))))))
           (->let (->plus (->literal 0)
                          (->literal 0))
                  (->let (->plus (->literal 1)
                                 (->literal 1))
                         (->let (->times (->variable 0 "fresh")
                                         (->variable 1 "fresh"))
                                (->let (->plus (->literal 2)
                                               (->literal 2))
                                       (->let (->plus (->literal 3)
                                                      (->literal 3))
                                              (->let (->times (->variable 3 "fresh")
                                                              (->variable 4 "fresh"))
                                                     (->let (->plus (->variable 2 "fresh")
                                                                    (->variable 5 "fresh"))
                                                            (->variable 6 "fresh")))))))))))
  (testing "reifyc-fac"
    (is (= (debug
            (reifyc #(evalms [] (->lift (->lambda
                                         (->if (->gt (->variable 1 "fresh") (->literal 0))
                                               (->times (->variable 1 "fresh")
                                                        (->apply (->variable 0 "fresh")
                                                                 (->minus (->variable 1 "fresh")
                                                                          (->lift (->literal 1)))))
                                               (->lift (->literal 1))))))))
           base-fac-anf))))


(comment
  ;; variable indices:
  ;; x3 -> 0
  ;; x -> 2
  ;; x1 -> 3
  ;; x2 -> 4
  (->code (let [x3 (fn _ [x]
                     (let [x1 (* x x)]
                       (let [x2 (+ x x1)]
                         x2)))]
            x3)))

(deftest paper-examples
  (testing "figure 4"
    (is (= (->code (->let (->lambda (->let (->times (->variable 1 "fresh") ;; TODO should have been "x"
                                                    (->variable 1 "fresh")) ;; TODO should have been "x"
                                           (->let (->plus (->variable 1 "fresh") ;; TODO should have been "x"
                                                          (->variable 2 "fresh")) ;; TODO could be called "fresh-plus"?
                                                  (->variable 3 "fresh")))) ;; TODO could be called "fresh-times"?
                          (->variable 0 "fresh")))
           (debug (evalmsg []
                           (->lift (->lambda (->plus (->variable 1 "x")
                                                     (->times (->variable 1 "x")
                                                              (->variable 1 "x")))))))))))

(comment
  (let [e (->lift (->lambda (->plus (->variable 1 "") (->times (->variable 0 "") (->variable 0 "")))))]
    (print-pattern (evalms [] e))))

(comment
  (testing "evalms reify"
    (is (= (debug (reify #(evalms [] p)))
           (->let (->lambda (->let (->literal 42) (->let (->literal 42) (->variable 1 ""))))
                  (->let (->apply (->variable 0 "") (->literal 3)) (->variable 1 "")))))))

