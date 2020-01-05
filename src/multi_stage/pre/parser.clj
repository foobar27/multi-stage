(ns multi-stage.pre.parser
  (:require [multi-stage.pre.ast :refer [->literal ->literal-vector ->literal-set ->literal-map
                                         ->fn ->apply ->do ->let ->if ->dot ->new ->throw
                                         ->class-reference ->variable-reference ->symbol-reference
                                         class-reference? variable-reference? symbol-reference?]
             :as pre-ast]
            [multi-stage.common.core :as common :refer [->variable unknown-source-context sexp->source-context
                                                        mockable-gensym]]
            [multi-stage.impl.core :refer [get-registered-global-variable]]
            [multi-stage.utils :refer [resolve-symbol var->sym unqualified-symbol?]]
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [macroexpand-all]]))

;; == Clojure sexp destrucuring

(defmulti destructure-clj (fn [sym args] sym))

(defmethod destructure-clj :default [_ args]
  nil)

(defmethod destructure-clj 'let* [_ args]
  (let [[bindings & bodies] args]
    {:bindings (partition 2 bindings)
     :bodies bodies}))

(defmethod destructure-clj 'do [_ args]
  {:bodies args})

(defmethod destructure-clj 'if [_ args]
  (let [[condition then else] args]
    {:condition condition
     :then then
     ;; else is nil by default. This is expected.
     :else else}))

(defmethod destructure-clj 'recur [_ arguments]
  {:arguments arguments})

(defmethod destructure-clj '. [_ args]
  args)

(defmethod destructure-clj 'new [_ [class-name & args]]
  {:class-name class-name
   :arguments args})

(defmethod destructure-clj 'throw [_ [arg]]
  {:exception arg})

(defmethod destructure-clj 'quote [_ [arg]]
  {:value arg})

(defmethod destructure-clj 'loop* [_ [bindings & bodies]]
  {:bindings (doall (map vec (partition 2 bindings)))
   :bodies bodies})

(comment ;; examples for fn*
  (([x] (clojure.core/+ x 1)))
  ([x] (clojure.core/+ x 1)))

(defmethod destructure-clj 'fn* [_ args]
  (let [;; add optional name (nil)
        [name & args] (if (symbol? (first args))
                        args
                        (into [nil] args))
        ;; add optional parameters for multiple-arity syntax
        arities (if (seq? (first args))
                  args
                  (list args))
        
        ;; parse arities into :args and :body
        arities (for [[args & bodies] arities]
                  {:args args
                   :bodies bodies})

        ;; group arities by #args
        arities (into  {} (for [arity arities]
                            (vec [(count (:args arity)) arity])))]
    {:name name
     :arities arities}))

;; == Handling of variables

(s/fdef generate-variable!
  :args (s/alt :1 (s/cat :original-symbol unqualified-symbol?)
               :2 (s/cat :original-symbol unqualified-symbol?
                         :source-context ::common/source-context)))
(defn generate-variable!
  "Generate a new variable, which is guaranteed to be unique. You need
  to provide a name which does not need to be unique, it will be used
  for debugging information only.

  You can also provide a source-context. If you don't, we try to guess
  it from the meta information of original-symbol.

  The original-symbol will be used during parsing to keep track of the
  variables in the current scope. So you need to use gensym to invent
  a variable which did not exist in the original source code."
  ([original-symbol]
   (generate-variable! original-symbol (sexp->source-context original-symbol)))
  ([original-symbol source-context]
   (->variable (mockable-gensym original-symbol)
               original-symbol
               source-context)))

;; While parsing clojure expressions, we need to keep track of which
;; variables are bound to which name in the current scope.

(defn- bind-variable
  "Bind a new variable to the current scope, and return the new scope."
  [scope new-variable]
  (assoc scope
         (common/variable->original-symbol new-variable)
         new-variable))

(defn- bind-variables [scope new-variables]
  "Bind a list of variables to the current scope and return the new
  scope."
  (reduce bind-variable scope new-variables))

;; We also need to keep track which loop or function a recur call
;; would refer to.

(defn- bind-recur-target
  "Return a new scope with a changed recur target. "
  [scope variable]
  (assoc scope ::recur-target variable))

(defn- get-recur-target
  "Get the recur target of the current scope."
  [scope]
  (get scope ::recur-target))

;; == Translate a clojure expression into a pre-AST.

;; In this process we transform a clojure expression into an abstract syntax tree.
;; This is still very close to clojure, except:
;;  - It's a proper AST, instead of loose sexps.
;;  - Symbols are resolved and transformed into unique variables
;;    (as defined in the common package). This includes removal of variable
;;    shadowing (via gensym).
;;  - Implicit do-statements are transformed into explicit do-statements.
;;  - Let statements bind one symbol at a time.
;;  - if-statements always have two brnaches (the second one will be a nil-statement
;;    if it's missing)
;;  - loop/recur blocks will be replaced by function definition/application
;;    (we expect the code generation step at the end of the pipeline to do tail-call
;;    elimintation and re-introduce loop/recur statements if appropriate).
;;  - Break down multiple arities of functions (by creating individual functions per arity).
;;  - Always try to keep the source-context of the original expression (via the
;;    meta information of the expression).

(declare sexp->pre)

(defmulti destructured-sexp->pre (fn [source-context sym destructured bound-values]
                                   sym))

(defn sexps->pre [sexps scope]
(map #(sexp->pre % scope) sexps))

(defn sexp->pre
  "Translate clojure into a pre-ast."
  [sexp scope]
  (let [source-context (sexp->source-context sexp)
        recur-item (fn [item]
                     (sexp->pre item scope))
        recur-items (fn [items]
                      (map recur-item items))]
    (cond
      ;; literal
      (or (string? sexp)
          (keyword? sexp)
          (char? sexp)
          (number? sexp)
          (nil? sexp)
          (contains? #{true false} sexp))
      (->literal source-context
                 sexp)

      (vector? sexp)
      (->literal-vector source-context
                        (recur-items sexp))

      (set? sexp)
      (->literal-set source-context
                     (recur-items sexp))

      (map? sexp)
      (->literal-map source-context
                     (for [[k v] sexp]
                       [(recur-item k) (recur-item v)]))

      (symbol? sexp)
      (or (if-let [variable (get scope sexp)]
            (->variable-reference source-context variable))
          (if-let [resolved-symbol (resolve-symbol sexp)]
            (if-let [global-variable (if (var? resolved-symbol)
                                       (get-registered-global-variable *ns* (var->sym resolved-symbol)))]
              (->variable-reference source-context global-variable)
              ;; The symbol is not defined in the scope which is
              ;; currently being translated. Maybe it is defined
              ;; outside of this scope.
              (cond
                (class? resolved-symbol)
                ;; sexp is already a qualified sybmol, the class reference
                (->class-reference source-context sexp)

                (var? resolved-symbol)
                (->symbol-reference source-context (var->sym resolved-symbol))

                true
                (throw (IllegalArgumentException. (str "Do not know what this symbol points to " resolved-symbol))))))
          (throw (IllegalArgumentException. (str "Unknown symbol: " sexp))))
      
      (seq? sexp)
      (if (seq sexp)
        ;; Not empty, we can split it into (f & args)
        (let [[f & args] sexp]
          (or
           ;; Does sexp start with a symbol?
           (when (symbol? f)
             (or
              ;; Try to destructure and see if we have a dedicated
              ;; multi-method defined for this case.
              (when-let [destructured (destructure-clj f args)]
                (destructured-sexp->pre source-context f destructured scope))
              ;; Function call on symbol. Parse this symbol into a
              ;; variable-reference, class-reference or
              ;; symbol-reference.
              (let [parsed-f (recur-item f)
                    parsed-args (recur-items args)]
                (if (or (variable-reference? parsed-f)
                        (symbol-reference? parsed-f))
                  (->apply source-context parsed-f parsed-args)
                  (throw (IllegalArgumentException. (str "Reference" parsed-f " cannot be at beginning of sexp.")))))))
           ;; The sexp does not start with a symbol so it might be a
           ;; function call on a more complex expression.
           (when-let [parsed-f (recur-item f)]
             (let [parsed-args (recur-items args)]
               (->apply source-context parsed-f parsed-args)))))
        ;; Empty list evaluates to an empty list.
        (->literal source-context '()))

      true
      (throw (IllegalArgumentException. (str "Don't know how to parse: " sexp))))))

(defmethod destructured-sexp->pre 'do [source-context _ {:keys [bodies]} scope]
  (let [bodies (or (seq (sexps->pre bodies scope))
                   ;; Empty implicit-do is equivalent to a nil literal.
                   (->literal source-context nil))]
    (condp = (count bodies)
      1 (first bodies) ;; skip do-block if only one statement
      (->do source-context
            bodies))))

(defn- build-nested-let [source-context body-source-context bindings body-fn scope new-arguments]
  (if (seq bindings)
    ;; Break out the first binding, and adjust the scope and the source-context.
    (let [[[k v] & bindings] bindings
          v (sexp->pre v scope) ;; must be done with old scope
          k (generate-variable! k)
          source-context (if (seq bindings)
                           ;; If there's a binding left, take the source-context of
                           ;; its symbol.
                           (sexp->source-context (first (first bindings)))
                           body-source-context)
          body (build-nested-let source-context
                                 body-source-context
                                 bindings ;; remaining bindings
                                 body-fn
                                 (bind-variable scope k)
                                 (conj new-arguments k))]
      (->let source-context k v body))
    ;; Base case of recursion (no bindings, implicit do).
    (body-fn source-context scope new-arguments)))

(defmethod destructured-sexp->pre 'let* [source-context _ {:keys [bindings bodies]} scope]
  (build-nested-let source-context
                    ;; body-source-context
                    (if (seq bodies)
                      ;; If we are at the base of the recursion,
                      ;; take the source-context of the first body
                      ;; (if available).
                      (sexp->source-context (first bodies))
                      ;; Else give up and take previous source-context
                      source-context)
                    bindings
                    (fn [source-context scope new-bindings]
                      (destructured-sexp->pre source-context
                                              'do
                                              {:bodies bodies}
                                              scope))
                    scope
                    []))

(comment
  (loop [x1 v1
         x2 v2 ;; might depend on x1
         x3 v3] ;; might depend on x1 and x2
    body)

  (let [x1 v1
        x2 v2
        x3 c3]
    ((fn [x1 x2 x3]
       body) y1 y2 y3)))
;; TODO some code is very similar to the let* case
(defmethod destructured-sexp->pre 'loop* [source-context _ {:keys [bindings bodies]} scope]
  (let [f-name (generate-variable! (mockable-gensym "loop") source-context)
        f (destructured-sexp->pre source-context
                                  'fn*
                                  {:name f-name
                                   :arities {(count bindings) {:args (vec (map first bindings))
                                                               :bodies bodies}}}
                                  scope)]
    ;; We need to introduce a let-block because the initial loop arguments
    ;; might depend on each other.
    (build-nested-let source-context
                      (if (seq bodies)
                        ;; If we are at the base of the recursion,
                        ;; take the source-context of the first body
                        ;; (if available).
                        (sexp->source-context (first bodies))
                        ;; Else give up and take previous source-context
                        source-context)
                      bindings
                      (fn [source-context scope new-arguments]
                        (->apply source-context
                                 f
                                 (map #(->variable-reference source-context %) new-arguments)))
                      scope
                      [])))

(defmethod destructured-sexp->pre 'recur [source-context _ {:keys [arguments]} scope]
  (->apply source-context
           (->variable-reference source-context (get-recur-target scope))
           (sexps->pre arguments scope)))

(defmethod destructured-sexp->pre 'fn* [source-context _ {:keys [name arities]} scope]
  (let [name (or (if (symbol? name)
                   (generate-variable! name)
                   name)
                 (generate-variable! (mockable-gensym "fn") source-context))
        [_ {:keys [args & bodies]}] (first arities) ;; TODO support several arities
        args (vec (map generate-variable! args))
        scope (-> scope
                  (bind-variables (conj args name))
                  (bind-recur-target name))
        body-source-context (if (seq bodies)
                              (sexp->source-context (first bodies))
                              ;; fn can have 0 bodies. Use the original source-context in this case.
                              source-context)]
    (->fn source-context
          name
          args
          (destructured-sexp->pre body-source-context
                                  'do
                                  {:bodies bodies}
                                  scope))))

(defmethod destructured-sexp->pre 'if [source-context _ {:keys [condition then else]} scope]
  (->if source-context
        (sexp->pre condition scope)
        (sexp->pre then scope)
        ;; If else is not given (aka nil), this will generate a literal nil, as expected.
        (sexp->pre else scope)))

(comment
  (macroexpand `(.writeByte output (int ~'data))))

;; TODO why does this destructuring not deliver key-values?
(defmethod destructured-sexp->pre '. [source-context _ [object method-name & arguments] scope]
  (->dot source-context
         (sexp->pre object scope)
         method-name
         (sexps->pre arguments scope)))

(defmethod destructured-sexp->pre 'new [source-context _ {:keys [class-name arguments]} scope]
  (->new source-context
         class-name
         (sexps->pre arguments scope)))

(defmethod destructured-sexp->pre 'throw [source-context _ {:keys [exception]} scope]
  (->throw source-context
           (sexp->pre exception scope)))

(defmethod destructured-sexp->pre 'quote [source-context _ {:keys [value]} scope]
  (->literal source-context
             value))

(defn clj->pre [sexp scope]
  (sexp->pre (macroexpand-all sexp)
             scope))
