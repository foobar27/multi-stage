(ns towers.base.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns]]))

;; We use de-bruijn indices to reference variables.
;; - lambdas all take one argument
;; - Var(N) -> look N back on stack to see which lambda an index is related to
;; - all lambdas have a self-reference as their first argument, to allow recursion
;; - see: https://en.wikipedia.org/wiki/De_Bruijn_index

(s/def ::environment vector?) ;; TODO

(s/def ::integer int?)

(defmultipattern expression)
(defpatterns expression
  literal  [n ::integer]
  literal? [e ::expression]
  variable [level ::integer original-name ::string] 
  lambda   [e ::expression]
  apply    [e1 ::expression e2 ::expression]
  equals?  [eq ::expression e2 ::expression]
  cons     [e1 ::expression e2 ::expression]
  cons?    [e1 ::expression]
  nil?     [e ::expression]
  empty?   [e ::expression]
  number?  [e ::expression]
  symbol?  [e ::expression]
  car      [e ::expression]
  cdr      [e ::expression]
  let      [e1 ::expression e2 ::expression]  ;; TODO allow destructuring?
  if       [c ::expression a ::expression b ::expression]
  plus     [a ::expression b ::expression] ;; TODO make variadic
  minus    [a ::expression b ::expression]
  times    [a ::expression b ::expression] ;; TODO make variadic
  gt       [a ::expression b ::expression]
  lt       [a ::expression b ::expression]
  ;; TODO other operators
  lift     [e ::expression]
  run      [b ::expression e ::expression])

;; TODO spec ::environment (atom of value?)

(defmultipattern value)
(defpatterns value
  ;; TODO why uppercase?
  TRUE  []
  FALSE  []
  NIL []
  constant  [n ::integer]
  tuple     [a ::value b ::value] ;; TODO make variadic
  closure   [env ::environment e ::expression]
  code      [e ::expression])
