(ns multi-stage.ir.value
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns]]
            [multi-stage.ir.ast :as ast]))

(s/def ::integer int)

(s/def ::environment vector?)

(defmultipattern value)
(defpatterns value
  constant  [value any?]
  closure   [arity integer?
             env ::environment
             body ::ast/expression
             original-function-symbol symbol?
             original-argument-symbols (s/coll-of symbol?)]
  code      [expression ::ast/expression])
