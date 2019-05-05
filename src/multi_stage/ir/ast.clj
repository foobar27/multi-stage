(ns multi-stage.ir.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns]]))

;; We use de-bruijn indices to reference variables.
;; - lambdas all take one argument
;; - Var(N) -> look N back on stack to see which lambda an index is related to
;; - all lambdas have a self-reference as their first argument, to allow recursion
;; - see: https://en.wikipedia.org/wiki/De_Bruijn_index


(s/def ::environment vector?) ;; TODO

(s/def ::integer int)
(s/def ::string string?)

(defmultipattern expression)
(defpatterns expression
  literal   [n any?]
  primitive-symbol [name symbol?]
  literal-vector   [elements (s/coll-of ::expression)]
  literal-set      [elements (s/coll-of ::expression)]
  literal-map      [elements (s/coll-of (s/tuple ::expression ::expression))]
  variable  [level ::integer]
  class-reference [class-name symbol?]
  let       [e1 ::expression e2 ::expression original-name symbol?]
  lambda    [arity integer?
             body ::expression
             original-function-name symbol?
             original-argument-names (s/coll-of symbol?)]
  apply     [e1 ::expression arguments (s/coll-of ::expression)]
  dot       [object ::expression method-name symbol? arguments (s/coll-of ::expression)]
  new       [class-name symbol? args (s/coll-of ::expression)]
  throw     [exception ::expression]
  quote     [form any?]
  do        [args (s/coll-of ::expression)]
  if        [c ::expression a ::expression b ::expression]
  lift      [e ::expression]
  run       [b ::expression e ::expression]
  primitive-call [sym qualified-symbol? args (s/coll-of ::expression)])

(defmultipattern value)
(defpatterns value
  constant  [value any?]
  closure   [arity integer?
             env ::environment
             body ::expression
             original-function-name symbol?
             original-argument-names (s/coll-of symbol?)]
  code      [expression ::expression])
