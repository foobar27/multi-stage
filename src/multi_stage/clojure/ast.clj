(ns multi-stage.clojure.ast
  (:require [clojure.spec.alpha :as s]
            [multi-stage.utils :refer [into!]]
            [meliae.patterns :refer [defmultipattern defpatterns match*]]))

(s/def ::signature
  (s/keys :req-un [::args ::bodies]))

(s/def ::let-binding
  (s/* (s/cat :symbol symbol?
              :expression ::expression)))

(defmultipattern expression)
(defpatterns expression
  literal  [value any?
            used-symbols (s/coll-of symbol? :kind sequential?)]
  literal-vector [elements (s/coll-of any?)
                  used-symbols (s/coll-of symbol? :kind sequential?)]
  literal-set    [elements (s/coll-of any?)
                  used-symbols (s/coll-of symbol? :kind sequential?)]
  literal-map    [elements (s/coll-of any?)
                  used-symbols (s/coll-of symbol? :kind sequential?)]
  variable [symbol symbol?
            used-symbols (s/coll-of symbol? :kind sequential?)]
  let*     [bindings (s/coll-of ::let-binding)
            bodies (s/coll-of ::expression)
            used-symbols (s/coll-of symbol? :kind sequential?)]
  throw    [exception ::expression
            used-symbols (s/coll-of symbol? :kind sequential?)]
  do       [bodies (s/coll-of ::expression)
            used-symbols (s/coll-of symbol? :kind sequential?)]
  if       [condition ::expression
            then ::expression else ::expression used-symbols (s/coll-of symbol? :kind sequential?)]
  fn*      [name symbol?
            signatures (s/coll-of ::signature)
            used-symbols (s/coll-of symbol? :kind sequential?)]
  dot      [object ::expression
            method-name symbol?
            arguments (s/coll-of ::expression)
            used-symbols (s/coll-of symbol? :kind sequential?)]
  class-reference [class-name symbol?
                   used-symbols (s/coll-of symbol? :kind sequential?)]
  new      [class-name symbol?
            arguments (s/coll-of ::expression)
            used-symbols (s/coll-of symbol? :kind sequential?)]
  invoke   [function ::expression
            args (s/coll-of ::expression)
            tail-position? boolean?
            used-symbols (s/coll-of symbol? :kind sequential?)])

(defn- merge-used-symbols-from [& expressions]
  (into [] (mapcat ::used-symbols expressions)))

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
      
      [(->invoke function args tail-position? used-symbols)]
      (->invoke function args false used-symbols)

      [(->do bodies used-symbols)]
      ;; By the smart constructor contract:
      ;; * bodies is not empty
      ;; * only the last body can contain a tail call (no need to check the others)
      (->do (remove-last-tail-position bodies))

      [(->let* bindings bodies used-symbols)]
      ;; By the smart constructor contract:
      ;; * bindings do not contain tail calls
      ;; * bodies is not empty
      ;; * only the last body can contain a tail call (no need to check the others)
      (->let* bindings
        (remove-last-tail-position bodies)
        used-symbols)

      [(->if condition then else used-symbols)]
      ;; By the smart constructor contract:
      ;; * condition does not contain tail calls (no need to check)
      (->if condition
            (remove-tail-position then)
            (remove-tail-position else)
            used-symbols)

      ;; Default: ignore
      [expression]
      expression)))

(defn- uses-symbol? [e s]
  (some #{s} (::used-symbols e)))

(declare smart-if)

(s/fdef smart-literal
  :args (s/cat :value any?)
  :ret  ::expression)
(defn smart-literal [value]
  (->literal value []))

(s/fdef smart-vector
  :args (s/cat :element (s/coll-of any?))
  :ret ::expression)
(defn smart-vector [elements]
  (->literal-vector elements (merge-used-symbols-from elements)))

(s/fdef smart-set
  :args (s/cat :element (s/coll-of any?))
  :ret ::expression)
(defn smart-set [elements]
  (->literal-set elements (merge-used-symbols-from elements)))

(s/fdef smart-map
  :args (s/cat :element (s/coll-of any?))
  :ret ::expression)
(defn smart-map [elements]
  (->literal-map (map vec elements) (merge-used-symbols-from elements)))

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
      (->literal nil [])

      ;; One single do body, flatten.
      ;; No recursion needed, since bodies2 had already been run through smart-do.
      ;; No need to change tail call booleans.
      [[(->do bodies2 used-symbols)]]
      (->do bodies2 used-symbols)

      ;; One single expression (not a do block), just return it.
      ;; No need to change tail call booleans.
      [[expression]]
      expression

      ;; More than one body, flatten nested bodies (one level).
      ;; No recursion needed, since there will be certainly more than one body.
      ;; The nested bodies have already beed constructed via smart-do.
      [_]
      (let [bodies (-> (mapcat (fn [body]
                                 (if (do? body)
                                   (::bodies body)
                                   [body]))
                               bodies)
                       adjust-tail-positions)]
        (->do bodies
              (apply merge-used-symbols-from bodies))))))

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
                                  [body])))
            build-let (fn [bindings bodies]
                        (->let* bindings
                          bodies
                          ;; Determine used symbols from
                          ;; - bindings (taking care of shadowing, incrementally for each binding)
                          ;; - bodies (without the symbols shadowed by the bindings)
                          (into (loop [used-syms (transient [])
                                       shadowed-syms #{}
                                       bindings bindings]
                                  (if (seq bindings)
                                    (let [[[k v] & remainder] bindings
                                          used-syms (into! used-syms (remove shadowed-syms (::used-symbols v)))
                                          shadowed-syms (conj shadowed-syms k)]
                                      (recur used-syms shadowed-syms remainder))
                                    (persistent! used-syms)))
                                ;; Remove the symbols from the bindings (shadowing).
                                (remove (set (map first bindings))
                                        (apply merge-used-symbols-from bodies)))))]
        (match* [body]

          ;; One single nested let* expression, merge into current expression.
          ;; By the smart constructor contract:
          ;; * bindings2 does not have tail calls
          ;; * bodies2 has already been treated for tail calls
          ;; * bindings2 is not empty
          ;; Thus we only need to remove tail calls from bindings, not from bindings2
          [(->let* bindings2 bodies2 used-symbols)]
          (build-let (concat (remove-tail-position-from-bindings bindings)
                             bindings2)
                     (simplify-bodies bodies2))

          ;; (let [... x 1] x)
          ;; We use recursion via smart-let*, so we don't need to worry
          ;; about tail calls right now.
          ;; Please note that in this case the last binding is a tail position.
          [(->variable last-binding-sym used-symbols)]
          (smart-let* (drop-last bindings)
            [last-binding-expr])

          ;; (let [... x 1] (if x then else))
          [(->if (->variable last-binding-sym used-symbols)
                 then
                 else
                 used-symbols-if)]
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
            (build-let (remove-tail-position-from-bindings bindings)
                       (simplify-bodies bodies)))
          
          ;; One single nested do expression, replace by implicit do.
          ;; No need to recur, if there has been a single nested let*,
          ;; it would be equal to body.
          [(->do bodies2 used-symbols)]
          (build-let (remove-tail-position-from-bindings bindings) bodies2)

          ;; A single expression (neither let* nor do).
          ;; This will be the single body of the new let*.
          ;; TODO do we need recursion?
          [expression]
          (build-let (remove-tail-position-from-bindings bindings)
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
   (smart-if condition then (->literal nil [])))
  ([condition then else]
   (let [condition (remove-tail-position condition)]
     (->if condition
           then
           else
           (merge-used-symbols-from condition then else)))))

(s/fdef smart-invoke
  :args (s/cat :f (s/alt :symbol symbol?
                         :lambda ::expression)
               :args (s/coll-of ::expression))
  :ret ::expression)
(defn smart-invoke [f args]
  (let [f (smart-do [f])
        args (doall (map #(remove-tail-position (smart-do [%])) args))]
    (->invoke f
              args
              ;; By default it's a tail call
              ;; Other smart constructors are responsible for setting this back to false
              true
              (into (apply merge-used-symbols-from args)
                    (merge-used-symbols-from f)))))

(s/fdef smart-dot
  :args (s/cat :object ::expression
               :method-name symbol?
               :arguments (s/coll-of ::expression))
  :ret ::expression)
(defn smart-dot [object method-name arguments]
  (->dot object
         method-name
         (vec arguments)
         (apply merge-used-symbols-from object arguments)))

(s/fdef smart-throw
  :args (s/cat :exception ::expression)
  :ret ::expression)
(defn smart-throw [exception]
  (->throw exception
           (merge-used-symbols-from exception)))

(s/fdef smart-class-reference
  :args (s/cat :class-name symbol?)
  :ret ::expression)
(defn smart-class-reference [class-name]
  (->class-reference class-name
                     []))

(s/fdef smart-new
  :args (s/cat :class-name symbol?
               :arguments (s/coll-of ::expression))
  :ret ::expression)
(defn smart-new [class-name arguments]
  (->new class-name
         (vec arguments)
         (apply merge-used-symbols-from arguments)))

(s/fdef smart-variable
  :args (s/cat :sym symbol?)
  :ret ::expression)
(defn smart-variable [sym]
  (->variable sym
              [sym]))

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
    (->fn* name
           signatures
           (apply merge-used-symbols-from
                  (mapcat (fn [{:keys [args bodies]}]
                            (remove (set args) (apply merge-used-symbols-from bodies)))
                          signatures)))))

