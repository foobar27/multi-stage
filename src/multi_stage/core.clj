(ns multi-stage.core
  (:refer-clojure :exclude [def defn defn- defmulti defmethod compile])
  (:require [multi-stage.impl.core :as impl]
            [multi-stage.common.core :as common :refer [sexp->source-context]]
            [multi-stage.pre.ast :as pre-ast]
            [multi-stage.pre.algorithms :as pre-algorithms]
            [multi-stage.pre.parser :refer [clj->pre]]
            [multi-stage.pre.free-variables :refer [pre->free-global-variables]]
            [multi-stage.ir.parser :as ir-parser]
            [multi-stage.ir.interpreter :as interpreter]
            [multi-stage.ir.generator :as ir-gen]
            [multi-stage.post.generator :as post-gen]
            [multi-stage.utils :refer [make-local-symbol resolve-symbol var->sym]]
            [clojure.spec.alpha :as s]))

(defmacro with-clean-definitions [& bodies]
  `(impl/with-clean-definitions ~@bodies))

(clojure.core/defn expand-and-register-def!
  "Abuse macro-expansion of defn by replacing ms-defn with def.
  Be careful to keep the meta data of the original &form.
  replacement should be a macro which expands to a def-form"
  [form replacement]
  (let [form (with-meta (macroexpand-1 `(~replacement ~@(rest form)))
               (meta form))
        source-context (sexp->source-context form)
        ;; TODO missing in the following destructuring: docstring
        [_ name-s value] form
        variable (common/->variable (common/mockable-gensym (name name-s)) ;; strip ns part
                                    (make-local-symbol name-s)
                                    source-context)
        _ (impl/register-variable! variable)
        value (or (if (seq value)
                    (if-let [[fn-sym & arities] value]
                      (if (and (= `fn fn-sym)
                               ;; There's no name yet
                               (not (symbol? (first arities))))
                        `(fn ~name-s ~@arities))))
                  value)
        value (clj->pre value {})
        dependencies (disj (into #{}
                                 (pre->free-global-variables value
                                                             (impl/determine-all-registered-variables)))
                           ;; Remove self reference to avoid recursion in subsequent algorithm.
                           variable)]

    
    (impl/register-definition! variable value dependencies)
    ;; TODO can we define this in a better way, with docstrings etc, such that editors help to do auto-complete?
    form))

(s/fdef def
  :args (s/alt :arity-1 (s/cat :symbol symbol?)
               :arity-2 (s/cat :symbol symbol?
                               :init any?)
               :arity-3 (s/cat :symbol symbol?
                               :docstring string?
                               :init any?))
  :ret any?)
(defmacro def
  "Like clojure.core/def, but to be used in multi-stage programs."
  {:arglists '([symbol docstring? init?])}
  [& args]
  (expand-and-register-def! &form 'def))

(s/fdef defn
  :args :clojure.core.specs.alpha/defn-args
  :ret any?)
(defmacro defn
  "Like clojure.core/defn, but to be used in multi-stage programs."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  [& args]
  (expand-and-register-def! &form 'clojure.core/defn))

(s/fdef defn-
  :args :clojure.core.specs.alpha/defn-args
  :ret any?)
(defmacro defn-
  "Like clojure.core/defn-, but to be used in multi-stage programs."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  [& args]
  (expand-and-register-def! &form 'clojure.core/defn-))

(defmacro compile [sym]
  (let [variable (or (impl/get-registered-global-variable *ns* (var->sym (resolve-symbol sym)))
                     (throw (RuntimeException. (str "Unable to resolve symbol in this context, did you define it with ms/def? " sym))))
        definition (impl/variable->definition variable)
        substitutions (into {}
                            (for [variable (impl/variable->sorted-dependencies variable)]
                              [variable (common/unqualify-and-duplicate-variable variable "-local")]))
        dependencies (for [variable (impl/variable->sorted-dependencies variable)
                           :let [local-variable (get substitutions variable)]]
                       [local-variable
                        (pre-algorithms/substitute-variables (impl/variable->definition variable)
                                                             substitutions)])
        [f-var f-definition] (last dependencies)
        f-source-context (common/variable->source-context f-var)
        f-var-inner (common/unqualify-and-duplicate-variable f-var "-inner")
        f-arg-vars (::pre-ast/arguments f-definition)
        pre (reduce (fn [expression [variable variable-definition]]
                      (pre-ast/->let (::pre-ast/source-context expression)
                                     variable
                                     variable-definition
                                     expression))
                    (pre-ast/->apply f-source-context
                                     (pre-ast/->variable-reference
                                      f-source-context
                                      f-var)
                                     (map #(pre-ast/->variable-reference f-source-context %)
                                          f-arg-vars))
                    (reverse dependencies))
        pre (pre-ast/->fn f-source-context
                          f-var-inner ;; TODO needs to be UNqualified
                          f-arg-vars
                          pre)
        ir (ir-parser/pre->ir pre {})
        ir (interpreter/evalmsg [] ir sym)
        value (-> ir
                  (ir-gen/generate nil)
                  (post-gen/generate nil))]
    `(def ~'sym ~value)))
