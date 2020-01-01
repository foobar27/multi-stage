(ns multi-stage.ir.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns]]))

;; The intermediate-representation (IR) AST is used by the main
;; multi-stage evaluation algorithm. It is usually built from a
;; pre-AST, and is a bit more low-level.

;; First of all, we use De Bruijn indices to reference variables.
;;
;; You can imagine the variables on a stack:
;; - a let-expression pushes a variable onto the stack
;; - a function definition pushes the function name to the stack
;;   (to allow the function body to refer to itself to do recursion)
;;   and then pushes each argument onto the stack.
;;
;; When we refer to a variable on a stack, via (->variable n),
;; we refer to the nth element from the top of the stack.
;; 
;; See the corresponding wikipedia article for a more generic
;; description https://en.wikipedia.org/wiki/De_Bruijn_index. Please
;; note however that we refer to the variables from the top of the
;; stack, whereas the wikipedia article refers to the variables from
;; the bottom of the stack.

;; Additionally, primitive function calls are replaced by a dedicated
;; IR-AST node to distinguish them from generic function calls (to
;; facilitate subsequent transformation steps).

(s/def ::integer int)

(defmultipattern expression)
(defpatterns expression
  literal          [value any?]
  primitive-symbol [name symbol?]
  literal-vector   [elements (s/coll-of ::expression)]
  literal-set      [elements (s/coll-of ::expression)]
  literal-map      [elements (s/coll-of (s/tuple ::expression ::expression))]
  variable         [level ::integer]
  class-reference  [class-name symbol?]
  let              [key-expression ::expression
                    value-expression ::expression
                    original-symbol symbol?]
  fn               [arity integer?
                    body ::expression
                    original-function-symbol symbol?
                    original-argument-symbols (s/coll-of symbol?)]
  apply            [function-expression ::expression
                    arguments (s/coll-of ::expression)]
  dot              [object ::expression
                    method-name symbol?
                    arguments (s/coll-of ::expression)]
  new              [class-name symbol?
                    arguments (s/coll-of ::expression)]
  throw            [exception ::expression]
  do               [args (s/coll-of ::expression)]
  if               [condition ::expression
                    true-branch ::expression
                    false-branch ::expression]
  lift             [expression ::expression]
  run              [body ::expression
                    stage ::expression])
