(ns multi-stage.base.parser-test
  (:require [multi-stage.base.parser :refer :all]
            [multi-stage.base.ast :refer [->lambda ->times ->plus ->minus ->variable ->literal ->let ->apply ->cons ->if
                                          ->TRUE ->equals? ->empty? ->car ->cdr]]
            [meliae.patterns :refer [print-pattern]]
            [multi-stage.test-utils :refer [verify-pattern remove-auto-gensym]]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]))

(stest/instrument (stest/enumerate-namespace ['meliae.patterns 'multi-stage.base.ast 'multi-stage.base.parser-test]))

(defmacro verify-parse [input expected]
  `(verify-pattern (remove-auto-gensym (parse ~input)) ~expected))

(deftest variadic-operators
  (testing "plus"
    (testing "(+ 1 2)"
      (verify-parse
       (+ 1 2)
       (->plus (->literal 1) (->literal 2))))
    (testing "(+ 1)"
      (verify-parse
       (+ 5)
       (->literal 5)))
    (testing "(+)"
      (verify-parse
       (+)
       (->literal 0)))
    (testing "(+ 1 2 3)"
      (verify-parse
       (+ 1 2 3)
       (->plus (->literal 1) (->plus (->literal 2) (->literal 3))))))
  (testing "times"
    (testing "(* 1 2)"
      (verify-parse
       (* 1 2)
       (->times (->literal 1) (->literal 2))))
    (testing "(* 1)"
      (verify-parse
       (* 5)
       (->literal 5)))
    (testing "(*)"
      (verify-parse
       (*)
       (->literal 1)))
    (testing "(* 1 2 3)"
      (verify-parse
       (* 1 2 3)
       (->times (->literal 1) (->times (->literal 2) (->literal 3)))))))

;; TODO also test destructuring fn
(deftest parse-fn
  (testing "(+ x (* x x))"
    (verify-parse
     (fn [x] (+ x (* x x)))
     (->lambda (->plus (->variable 1 "x")
                       (->times (->variable 1 "x") (->variable 1 "x"))))))
  (testing "2 nested lambdas (de bruijn indices)"
    (verify-parse
     (fn [x] (fn [y] (+ x (* y y))))
     (->lambda (->lambda (->plus (->variable 1 "x")
                                 (->times (->variable 3 "y")
                                          (->variable 3 "y")))))))
  (testing "fn without argument"
    (verify-parse
     (fn [] (+ 1 2))
     (->lambda (->plus (->literal 1) (->literal 2)))))
  (testing "fn with 2 arguments"
    (verify-parse
     (fn [x y] (+ x y))
     (->lambda (->lambda (->plus (->variable 1 "x") (->variable 3 "y"))))))
  (testing "fn with 3 arguments"
    (verify-parse
     (fn [x y z] (+ x (* y z)))
     (->lambda (->lambda (->lambda (->plus (->variable 1 "x")
                                           (->times (->variable 3 "y") (->variable 5 "z")))))))))

(deftest fn*-via-read-macro
  ;; This usess the syntax (fn* [x] (+ x 1))
  ;; (without the parenthesis which would be used for multiple arities)
  (testing "anon fn"
    (verify-parse
     #(+ % 1)
     (->lambda (->plus (->variable 1 "p1")
                       (->literal 1))))))


;; TODO also test destructuring let
(deftest parse-let
  (testing "Simple let"
    (verify-parse
     (let [x 1] (+ x x))
     (->let (->literal 1)
            (->plus (->variable 0 "x") (->variable 0 "x")))))
  (testing "Empty let should be discarded"
    (verify-parse
     (let [] (+ 1 2))
     (->plus (->literal 1) (->literal 2))))
  (testing "Let with two independent bindings"
    (verify-parse
     (let [x 1 y 2] (+ x y))
     (->let (->literal 1)
            (->let (->literal 2)
                   (->plus (->variable 0 "x")
                           (->variable 1 "y"))))))
  (testing "Let with two dependent bindings"
    (verify-parse
     (let [x 1 y x] (+ x y))
     (->let (->literal 1)
            (->let (->variable 0 "x")
                   (->plus (->variable 0 "x") (->variable 1 "y")))))))

(deftest apply-lambda
  (testing "apply+lambda, 1 argument"
    (verify-parse
     ((fn [x] (+ 1 x)) 5)
     (->apply (->lambda (->plus (->literal 1) (->variable 1 "x")))
              (->literal 5))))
  (testing "apply+lambda, 2 argument"
    (verify-parse
     ((fn [x y] (+ x (* x y))) 3 4)
     (->apply (->apply (->lambda (->lambda (->plus (->variable 1 "x")
                                                   (->times (->variable 1 "x")
                                                            (->variable 3 "y")))))
                       (->literal 3))
              (->literal 4))))
  (testing "apply+lambda, 0 arguments"
    (verify-parse
     ((fn [] 1))
     (->apply (->lambda (->literal 1))
              (->literal 0)))))

(deftest apply-fn
  (testing "apply+fn, 1 argument"
    (verify-parse
     (let [f (fn [x] (+ x 1))]
       (f 5))
     (->let (->lambda (->plus (->variable 1 "x") (->literal 1)))
            (->apply (->variable 0 "f")
                     (->literal 5))))))

(deftest recursive-apply-fn
  (testing "recursive apply fn"
    (verify-parse
     ((fn f [x] (f x)) 5)
     (->apply (->lambda (->apply (->variable 0 "f") (->variable 1 "x")))
              (->literal 5)))))

(deftest scoping
  (testing "override + with *"
    (verify-parse
     (let [+ (fn [x y] (* x y))]
       (+ 5 2))
     (->let (->lambda (->lambda (->times (->variable 1 "x") (->variable 3 "y"))))
            (->apply (->apply (->variable 0 "+")
                              (->literal 5))
                     (->literal 2))))))

(deftest cons-statement
  (testing "(fn [a b] (cons a b))"
    (verify-parse
     (fn [a b] (cons a b))
     (->lambda (->lambda (->cons (->variable 1 "a") (->variable 3 "b")))))))

(deftest if-statement
  (testing "if then else"
    (verify-parse
     (if 42 3 4)
     (->if (->literal 42)
           (->literal 3)
           (->literal 4)))))


(deftest =-statement
  (testing "(= 1)"
    (verify-parse
     (= 1)
     (->TRUE)))
  (testing "(= 1 2)"
    (verify-parse
     (= 1 2)
     (->equals? (->literal 1)
                (->literal 2)))))

(deftest fac-function
  (testing "fac-function"
    (verify-parse
     (fn fac [n]
       (if n
         (* n (fac (- n 1)))
         1))
     (->lambda
      (->if (->variable 1 "n")
            (->times (->variable 1 "n")
                     (->apply (->variable 0 "fac")
                              (->minus (->variable 1 "n")
                                       (->literal 1))))

            (->literal 1))))))
