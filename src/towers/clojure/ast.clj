(ns towers.clojure.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns match*]]))

(defmultipattern expression)
(defpatterns expression
  literal  [value any?]
  variable [symbol symbol?]
  let*     [bindings (coll-of ::let-binding) bodies (coll-of ::expression)]
  do       [bodies (coll-of ::expression)]
  if       [condition ::expression then ::expression else ::expression]
  invoke   [function ::expression args (coll-of ::expression)])

;; TODO document guarantees
(s/fdef smart-do
  :args (s/cat :bodies (s/coll-of ::expression))
  :ret ::expression)
(defn smart-do [bodies]
  (match* [bodies]
    ;; No body, return nil literal.
    [[]]
    (->literal nil)

    ;; One single do body, flatten.
    ;; No recursion needed, since bodies2 had already been run through smart-do.
    [[(->do bodies2)]]
    (->do bodies2)

    ;; One single expression (not a do block), just return it.
    [[expression]]
    expression

    ;; More than one body, flatten nested bodies (one level).
    ;; No recursion needed, since there will be certainly more than one body.
    ;; The nested bodies have already beed constructed via smart-do.
    [_]
    (->do (mapcat (fn [body]
                    (if (do? body)
                      (::bodies body)
                      [body]))
                  bodies))))

;; TODO document guarantees
(s/fdef smart-let*
  :args (s/cat :bindings (s/coll-of (s/tuple symbol? ::expression))
               :bodies   (s/coll-of ::expression))
  :ret ::expression)
(defn smart-let* [bindings bodies]
  (if (seq bindings)
    (match* [bodies]

      ;; One single nested let* expression, merge into current expression.
      ;; Simplify bodies2, but do not recur smart-let*.
      [[(->let* bindings2 bodies2)]]
      (->let* (concat bindings bindings2) (smart-do bodies2))

      ;; One single do expression, eliminate.
      ;; Recur, because the bodies might contain another let*.
      [[(->do bodies2)]]
      (smart-let* bindings bodies2)

      ;; A single expression (neither let* nor do).
      ;; This will be the single body of the new let*.
      ;; TODO do we need recursion?
      [[expression]]
      (->let* bindings expression)

      ;; Zero, or more than one bodies.
      ;; Flatten nested do blocks via the smart-do constructor, then recur.
      ;; This will always return a single expression (a do, a let*, a nil literal or some other block).
      ;; Thus it will match another case of this match* statement,
      ;; and thus not lead to an infinite recursion.
      [_]
      (smart-let* bindings (smart-do bodies)))
    ;; No bindings, consider as a do-block.
    (smart-do bodies)))

(s/fdef smart-if
  :args (s/alt :condition-then      (s/cat :condition ::expression
                                           :then      ::expression)
               :condition-then-else (s/cat :condition ::expression
                                           :then      ::expression
                                           :else      ::expression))
  :ret ::expression)
(defn smart-if
  ([condition then]
   (smart-if condition then (->literal nil)))
  ([condition then else]
   (->if condition then else)))
