(ns towers.ir.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns]]))

;; We use de-bruijn indices to reference variables.
;; - lambdas all take one argument
;; - Var(N) -> look N back on stack to see which lambda an index is related to
;; - all lambdas have a self-reference as their first argument, to allow recursion
;; - see: https://en.wikipedia.org/wiki/De_Bruijn_index


(s/def ::environment vector?) ;; TOOD

(s/def ::integer int)
(s/def ::string string?)

(defmultipattern expression)
(defpatterns expression
  literal   [n any?]
  variable  [level ::integer original-name ::string]
  class-reference [class-name symbol?]
  let       [e1 ::expression e2 ::expression]
  lambda    [e ::expression]
  apply     [e1 ::expression arguments (s/coll-of ::expression)]
  dot       [object ::expression method-name symbol? arguments (s/coll-of ::expression)]
  new       [class-name symbol? args (coll-of ::expression)]
  throw     [exception ::expression]
  quote     [form ::expression]
  do        [args (s/coll-of ::expression)]
  if        [c ::expression a ::expression b ::expression]
  lift      [e ::expression]
  run       [b ::expression e ::expression]
  primitive-call [sym qualified-symbol? args (coll-of ::expression)])

(defmultipattern value)
(defpatterns value
  ;; TODO why uppercase?
  TRUE  []
  FALSE  []
  NIL []
  constant  [value ::integer]
  closure   [env ::environment e ::expression]
  code      [expression ::expression])
