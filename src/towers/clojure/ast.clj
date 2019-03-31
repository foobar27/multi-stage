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
  loop*    [bindings (coll-of ::let-binding) bodies (coll-of ::expression)]
  throw    [exception ::expression]
  do       [bodies (coll-of ::expression)]
  if       [condition ::expression then ::expression else ::expression]
  fn*      [name symbol? signatures (coll-of ::signatures)]
  call     [function ::expression arguments (coll-of ::expression)]
  dot      [object ::expression method-name symbol? arguments (s/coll-of ::expression)]
  new      [class-name symbol? arguments (s/coll-of ::expression)]
  throw    [exception ::expression]
  invoke   [function ::expression args (coll-of ::expression) tail-call? boolean?])

;; TODO what about  "return nil" after tail call?
;; TODO this does not work if condition is a do/let/if-statement (with at least 2 bodies, the second one being a tail call)
;; TODO maybe we should remember "has-tail-call" if last do-statement is a tail-call? but this would hinder pattern matching in the generator
(defn- remove-tail-call
  "Resets tail call flags in invoke, do-blocks and implicit do-blocks."
  [e]
  (letfn [(remove-last-tail-call [bodies]
            (conj (vec (butlast bodies))
                  (remove-tail-call (last bodies))))]
    (match* [e]
      
      [(->invoke function args tail-call?)]
      (->invoke function args false)

      [(->do bodies)]
      ;; By the smart constructor contract:
      ;; * bodies is not empty
      ;; * only the last body can contain a tail call.
      (->do (remove-last-tail-call bodies))

      [(->let* bindings bodies)]
      ;; By the smart constructor contract:
      ;; * bindings do not contain tail calls
      ;; * bodies is not empty
      ;; * only the last body can contain a tail call
      (->let* bindings
        (remove-last-tail-call bodies))

      [(->if condition then else)]
      ;; By the smart constructor contract:
      ;; * condition does not contain tail calls
      (->if condition
            (remove-tail-call then)
            (remove-tail-call else))

      ;; Default: ignore
      [expression]
      expression)))

(declare smart-if)

(s/fdef smart-literal
  :args (s/cat :value any?)
  :ret  ::expression)
(defn smart-literal [value]
  (->literal value))

(s/fdef smart-do
  :args (s/cat :bodies (s/coll-of ::expression))
  :ret ::expression)
(defn smart-do [bodies]
  (letfn [(adjust-tail-calls [bodies]
            (if (seq bodies)
              (conj (vec (map remove-tail-call (butlast bodies)))
                    ;; keep the tail of the last one
                    (last bodies))))]
    (match* [(vec bodies)]
      ;; No body, return nil literal.
      [[]]
      (->literal nil)

      ;; One single do body, flatten.
      ;; No recursion needed, since bodies2 had already been run through smart-do.
      ;; No need to change tail call booleans.
      [[(->do bodies2)]]
      (->do bodies2)

      ;; One single expression (not a do block), just return it.
      ;; No need to change tail call booleans.
      [[expression]]
      expression

      ;; More than one body, flatten nested bodies (one level).
      ;; No recursion needed, since there will be certainly more than one body.
      ;; The nested bodies have already beed constructed via smart-do.
      [_]
      (->do (-> (mapcat (fn [body]
                          (if (do? body)
                            (::bodies body)
                            [body]))
                        bodies)
                adjust-tail-calls)))))

;; TODO document guarantees
(s/fdef smart-let*
  :args (s/cat :bindings (s/coll-of (s/tuple symbol? ::expression))
               :bodies   (s/coll-of ::expression))
  :ret ::expression)
(defn smart-let* [bindings bodies]
  ;; Simplify bodies, it's easier to reason about a single body.
  ;; This way we also do not need to remove the (butlast) tailcalls from the bodies.
  (let [body (smart-do bodies)]
    (if (seq bindings)
      (let [[last-binding-sym last-binding-expr] (last bindings)]
        (match* [body]

          ;; One single nested let* expression, merge into current expression.
          ;; By the smart constructor contract:
          ;; * bindings2 does not have tail calls
          ;; * bodies2 has already been treated for tail calls
          ;; * bindings2 is not empty
          ;; Thus we only need to remove tail calls from bindings, not from bindings2
          [(->let* bindings2 bodies2)]
          (->let* (concat (map remove-tail-call bindings) bindings2)
            [(smart-do bodies2)])

          ;; (let [... x 1] x)
          ;; We use recursion via smart-let*, so we don't need to worry
          ;; about tail calls right now.
          ;; Please note that in this case the last binding is a tail position.
          [(->variable last-binding-sym)]
          (smart-let* (drop-last bindings)
            [last-binding-expr])

          ;; (let [... x 1] (if x then else))
          ;; We use recursion via smart-let*, so we don't need to worry
          ;; about tail calls right now.
          ;; Please note that in this case the last binding is *not* a tail position,
          ;; but the smart-if constructor takes care of that.
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
          (->let* (map remove-tail-call bindings) bodies2)

          ;; A single expression (neither let* nor do).
          ;; This will be the single body of the new let*.
          ;; TODO do we need recursion?
          [expression]
          (->let* (map remove-tail-call bindings) [expression])))
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
   (->if (remove-tail-call condition)
         then
         else)))

(s/fdef smart-invoke
  :args (s/cat :f (s/alt :symbol symbol?
                         :lambda ::expression)
               :args (s/coll-of ::expression))
  :ret ::expression)
(defn smart-invoke [f args]
  (->invoke (smart-do [f])
            (doall (map #(remove-tail-call (smart-do [%])) args))
            ;; by default it's a tail call
            ;; other smart constructors are responsible for setting this back to false
            true))

(s/fdef smart-variable
  :args (s/cat :sym symbol?)
  :ret ::expression)
(defn smart-variable [sym]
  (->variable sym))

;; TODO implicit do, and remove tail calls from that implicit do
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
