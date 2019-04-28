(ns towers.clojure.ast
  (:require [clojure.spec.alpha :as s]
            [meliae.patterns :refer [defmultipattern defpatterns match*]]))

(s/def ::signature
  (s/keys :req-un [::args ::bodies]))

(s/def ::let-binding
  (s/* (s/cat :symbol symbol?
              :expression ::expression)))

(defmultipattern expression)
(defpatterns expression
  literal  [value any?]
  variable [symbol symbol?]
  let*     [bindings (s/coll-of ::let-binding) bodies (s/coll-of ::expression)]
  loop*    [bindings (s/coll-of ::let-binding) bodies (s/coll-of ::expression)]
  throw    [exception ::expression]
  do       [bodies (s/coll-of ::expression)]
  if       [condition ::expression then ::expression else ::expression]
  fn*      [name symbol? signatures (s/coll-of ::signature)]
  dot      [object ::expression method-name symbol? arguments (s/coll-of ::expression)]
  class-reference [class-name symbol?]
  new      [class-name symbol? arguments (s/coll-of ::expression)]
  invoke   [function ::expression args (s/coll-of ::expression) tail-position? boolean?])

;; TODO what about "return nil" after tail call?
;; TODO this does not work if condition is a do/let/if-statement (with at least 2 bodies, the second one being a tail call)
;; TODO maybe we should remember "has-tail-position" if last do-statement is a tail-position? but this would hinder pattern matching in the generator
(defn- remove-tail-position
  "Resets tail call flags in invoke, do-blocks and implicit do-blocks."
  [e]
  (letfn [(remove-last-tail-position [bodies]
            (conj (vec (butlast bodies))
                  (remove-tail-position (last bodies))))]
    (match* [e]
      
      [(->invoke function args tail-position?)]
      (->invoke function args false)

      [(->do bodies)]
      ;; By the smart constructor contract:
      ;; * bodies is not empty
      ;; * only the last body can contain a tail call (no need to check the others)
      (->do (remove-last-tail-position bodies))

      [(->let* bindings bodies)]
      ;; By the smart constructor contract:
      ;; * bindings do not contain tail calls
      ;; * bodies is not empty
      ;; * only the last body can contain a tail call (no need to check the others)
      (->let* bindings
        (remove-last-tail-position bodies))

      [(->if condition then else)]
      ;; By the smart constructor contract:
      ;; * condition does not contain tail calls (no need to check)
      (->if condition
            (remove-tail-position then)
            (remove-tail-position else))

      ;; Default: ignore
      [expression]
      expression)))

(defn- uses-symbol? [e s]
  (letfn [(uses-this-symbol? [e]
            (uses-symbol? e s))]
    (match* [e]
      [(->invoke f args tail-position?)]
      (or (= f s)
          (some uses-this-symbol? args))

      [(->literal value)]
      false

      [(->variable sym)]
      (= s sym)

      [(->let* bindings bodies)]
      ;; TODO one of the bindings could shadow subsequent usages
      (or (some uses-this-symbol? (map second bindings))
          (some uses-this-symbol? bodies))
      
      [(->loop* bindings bodies)]
      ;; TODO one of the bindings could shadow subsequent usages
      (or (some uses-this-symbol? (map second bindings))
          (some uses-this-symbol? bodies))
      
      [(->throw exception)]
      (uses-this-symbol? exception)

      [(->do bodies)]
      (some uses-this-symbol? bodies)

      [(->if condition then else)]
      (or (uses-this-symbol? condition)
          (uses-this-symbol? then)
          (uses-this-symbol? else))

      [(->fn* name signatures)]
      (some (fn [{:keys [args bodies]}]
              ;; TODO one arg could shadow the signature
              (some uses-this-symbol? bodies))
            signatures)

      [(->dot object method-name arguments)]
      (or (uses-this-symbol? object)
          (some uses-this-symbol? arguments))

      [(->class-reference class-name)]
      false

      [(->new class-name arguments)]
      (some uses-this-symbol? arguments)

      [(->invoke function args tail-position?)]
      (or (uses-this-symbol? function)
          (some uses-this-symbol? args)))))

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
  (letfn [(adjust-tail-positions [bodies]
            (if (seq bodies)
              (conj (vec (map remove-tail-position (butlast bodies)))
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
                adjust-tail-positions)))))

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
      (let [[last-binding-sym last-binding-expr] (last bindings)
            remove-tail-position-from-bindings (fn [bindings]
                                                 (for [[k v] bindings]
                                                   [k (remove-tail-position v)]))
            simplify-bodies (fn [bodies]
                              (let [body (smart-do bodies)]
                                (if (do? body)
                                  (::bodies body)
                                  [body])))]
        (match* [body]

          ;; One single nested let* expression, merge into current expression.
          ;; By the smart constructor contract:
          ;; * bindings2 does not have tail calls
          ;; * bodies2 has already been treated for tail calls
          ;; * bindings2 is not empty
          ;; Thus we only need to remove tail calls from bindings, not from bindings2
          [(->let* bindings2 bodies2)]
          (->let* (concat (remove-tail-position-from-bindings bindings)
                          bindings2)
            (simplify-bodies bodies2))

          ;; (let [... x 1] x)
          ;; We use recursion via smart-let*, so we don't need to worry
          ;; about tail calls right now.
          ;; Please note that in this case the last binding is a tail position.
          [(->variable last-binding-sym)]
          (smart-let* (drop-last bindings)
            [last-binding-expr])

          ;; (let [... x 1] (if x then else))
          [(->if (->variable last-binding-sym)
                 then
                 else)]
          (if-not (or (uses-symbol? then last-binding-sym)
                      (uses-symbol? else last-binding-sym))
            ;; We use recursion via smart-let*, so we don't need to worry
            ;; about tail calls right now.
            ;; Please note that in this case the last binding is *not* a tail position,
            ;; but the smart-if constructor takes care of that.
            (smart-let* (drop-last bindings)
              [(smart-if last-binding-expr
                         then
                         else)])
            ;; Unfortunately we cannot do this optimization because the symbol
            ;; is used at another location too.
            (->let* (remove-tail-position-from-bindings bindings)
              (simplify-bodies bodies)))
          
          ;; One single nested do expression, replace by implicit do.
          ;; No need to recur, if there has been a single nested let*,
          ;; it would be equal to body.
          [(->do bodies2)]
          (->let* (remove-tail-position-from-bindings bindings) bodies2)

          ;; A single expression (neither let* nor do).
          ;; This will be the single body of the new let*.
          ;; TODO do we need recursion?
          [expression]
          (->let* (remove-tail-position-from-bindings bindings)
            [expression])))
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
   (->if (remove-tail-position condition)
         then
         else)))

(s/fdef smart-invoke
  :args (s/cat :f (s/alt :symbol symbol?
                         :lambda ::expression)
               :args (s/coll-of ::expression))
  :ret ::expression)
(defn smart-invoke [f args]
  (->invoke (smart-do [f])
            (doall (map #(remove-tail-position (smart-do [%])) args))
            ;; by default it's a tail call
            ;; other smart constructors are responsible for setting this back to false
            true))

(s/fdef smart-dot
  :args (s/cat :object ::expression
               :method-name symbol?
               :arguments (s/coll-of ::expression))
  :ret ::expression)
(defn smart-dot [object method-name arguments]
  (->dot object method-name (vec arguments)))

(s/fdef smart-throw
  :args (s/cat :exception ::expression)
  :ret ::expression)
(defn smart-throw [exception]
  (->throw exception))

(s/fdef smart-class-reference
  :args (s/cat :class-name symbol?)
  :ret ::expression)
(defn smart-class-reference [class-name]
  (->class-reference class-name))

(s/fdef smart-new
  :args (s/cat :class-name symbol?
               :arguments (s/coll-of ::expression))
  :ret ::expression)
(defn smart-new [class-name arguments]
  (->new class-name arguments))

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

