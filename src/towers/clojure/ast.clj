(ns towers.clojure.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns match*]]))

(s/def ::signature
  (s/keys :req-un [::args ::bodies]))

(defmultipattern expression)
(defpatterns expression
  literal  [value any?]
  variable [symbol symbol?]
  let*     [bindings (coll-of ::let-binding) bodies (coll-of ::expression)]
  do       [bodies (coll-of ::expression)]
  if       [condition ::expression then ::expression else ::expression]
  fn*      [name symbol? signatures (coll-of ::signatures)]
  invoke   [function ::expression args (coll-of ::expression)])

(declare smart-if)

(s/fdef smart-literal
  :args (s/cat :value any?)
  :ret  ::expression)
(defn smart-literal [value]
  (->literal value))

;; TODO document guarantees
(s/fdef smart-do
  :args (s/cat :bodies (s/coll-of ::expression))
  :ret ::expression)
(defn smart-do [bodies]
  (match* [(vec bodies)]
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
  ;; Simplify bodies, it's easier to reason about a single body.
  (let [body (smart-do bodies)]
    (if (seq bindings)
      (let [[last-binding-sym last-binding-expr] (last bindings)]
        (match* [body]

          ;; One single nested let* expression, merge into current expression.
          [(->let* bindings2 bodies2)]
          (->let* (concat bindings bindings2)
            [(smart-do bodies2)])

          ;; (let [... x 1] x)
          [(->variable last-binding-sym)]
          (smart-let* (drop-last bindings)
            [last-binding-expr])

          ;; (let [... x 1] (if x then else))
          [(->if (->variable last-binding-sym)
                 then
                 else)]
          (smart-let* (drop-last bindings)
            [(smart-if last-binding-expr
                       then
                       else)])
          
          ;; One single nested do expression, replace by implicit do.
          ;; No need to recur, if there has been a single nested let*,
          ;; it would be equal to body.
          [(->do bodies2)]
          (->let* bindings bodies2)

          ;; A single expression (neither let* nor do).
          ;; This will be the single body of the new let*.
          ;; TODO do we need recursion?
          [expression]
          (->let* bindings [expression])))
      ;; No bindings, consider as a do-block.
      body)))

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
   (->if condition
         then
         else)))

(s/fdef smart-invoke
  :args (s/cat :f (s/alt :symbol symbol?
                         :lambda ::expression)
               :args (s/coll-of ::expression))
  :ret ::expression)
(defn smart-invoke [f args]
  (->invoke (smart-do [f])
            (doall (map #(smart-do [%]) args))))

(s/fdef smart-variable
  :args (s/cat :sym symbol?)
  :ret ::expression)
(defn smart-variable [sym]
  (->variable sym))

;; TODO implicit do
(s/fdef smart-fn*
  :args (s/alt :named   (s/cat :sym symbol?
                               :signatures (s/+ ::signature))
               :unnamed (s/cat :sigantures (s/+ ::signature)))
  :ret ::expression)
(defn smart-fn* [& chunks]
  (let [named-variant? (symbol? (first chunks))
        name (if named-variant?
               (first chunks))
        signatures (if named-variant?
                     (rest chunks)
                     chunks)]
    (->fn* name signatures)))
