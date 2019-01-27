(ns towers.clojure-parser-test
  (:require [clojure.test :refer :all]
            [clojure.walk :refer [macroexpand-all]]
            [towers.test-utils :refer [remove-auto-gensym]]
            [towers.clojure-parser :refer :all]))

(deftest function-argument-parsing
  (are [expected input]
      (= expected
         (destructure-fn* (rest (remove-auto-gensym (macroexpand-all input)))))

    ;; fn, 0 args
    {:name nil
     :arities {0 {:args []
                  :body [1]}}}
    `(fn [] 1)

    {:name 'foo
     :arities {0 {:args []
                  :body [1]}}}
    `(fn ~'foo [] 1)

    ;; fn, 1 args
    {:name nil
     :arities {1 {:args ['x]
                  :body [`(+ ~'x 1)]}}}
    `(fn [~'x] (+ ~'x 1))

    {:name 'foo
     :arities {1 {:args ['x]
                  :body [`(+ ~'x 1)]}}}
    `(fn ~'foo [~'x] (+ ~'x 1))

    ;; fn, 2 arities
    {:name nil
     :arities {1 {:args ['x]
                  :body [`(+ ~'x 1)]}
               2 {:args ['x 'y]
                  :body [`(+ ~'x ~'y)]}}}
    `(fn
       ([~'x] (+ ~'x 1))
       ([~'x ~'y] (+ ~'x ~'y)))

    {:name 'foo
     :arities {1 {:args ['x]
                  :body [`(+ ~'x 1)]}
               2 {:args ['x 'y]
                  :body [`(+ ~'x ~'y)]}}}
    `(fn ~'foo
       ([~'x] (+ ~'x 1))
       ([~'x ~'y] (+ ~'x ~'y)))

    ;; reader macro, 0 args
    {:name nil
     :arities {0 {:args []
                  :body ['(42)]}}}
    `#(42)
    
    ;; reader macro, 1 arg
    {:name nil
     :arities {1 {:args ['p1]
                  :body [`(+ ~'p1 1)]}}}
    `#(+ % 1)

    ;; reader macro, 2 args
    {:name nil
     :arities {2 {:args ['p1 'p2]
                  :body [`(+ ~'p1 ~'p2)]}}}
    `#(+ %1 %2))
  
  ;; TODO what about docstrings, pre- and post-conditions?
  ;; TODO test cases for implicit do
  ;; TODO support fully variadic (there can only be 1 such arity)
  )
