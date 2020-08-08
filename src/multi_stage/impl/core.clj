(ns multi-stage.impl.core
  (:require [multi-stage.utils :refer [make-local-symbol]]
            [multi-stage.common.core :as common :refer [sexp->source-context mockable-gensym]]
            [multi-stage.common.core :refer [->variable]]
            [multi-stage.pre.ast :as pre-ast]
            [multi-stage.pre.algorithms :as pre-algorithms]
            [multi-stage.pre.free-variables :refer [pre->free-global-variables]]
            [clojure.spec.alpha :as s]))

;; # Handling of definitions

(def ^:private empty-definitions
  {::symbol->variable {}
   ::variable->definition {}
   ::variable->dependencies {}
   ::symbol->meta {}
   ::compiled-symbols #{}})

(defonce ^:dynamic *registered-definitions* (atom empty-definitions))

(defn determine-all-registered-variables []
  (vals (get @*registered-definitions* ::symbol->variable)))

(defmacro with-clean-definitions [& bodies]
  `(with-bindings {#'*registered-definitions* (atom empty-definitions)}
     ~@bodies))

(defn reset-definitions! []
  (reset! *registered-definitions* empty-definitions))

(comment
  (reset-definitions!))

(defn register-definition! [variable value dependencies]
  (swap! *registered-definitions*
         assoc-in
         [::variable->definition variable]
         value)
  (swap! *registered-definitions*
         assoc-in
         [::variable->dependencies variable]
         dependencies))

(defn register-variable! [variable]
  (let [original-symbol (::common/original-symbol variable)]
    (swap! *registered-definitions* assoc-in
           [::symbol->variable original-symbol]
           variable)
    (swap! *registered-definitions* assoc-in
           [::symbol->meta (meta original-symbol)]
           variable)
    variable))

(s/fdef register-compiled-symbol!
  :args (s/cat :symbol qualified-symbol?)
  :ret any?)
(defn register-compiled-symbol! [symbol]
  (swap! *registered-definitions* update
         ::compiled-symbols
         conj
         symbol))

(s/fdef variable->definition
  :args (s/cat :variable ::common/variable)
  :ret any?)
(defn variable->definition
  "Gets a previously registered definition, as a pair of key and value.
  The key is the name, with the original meta information.
  The value is the original form."
  [variable]
  (get-in @*registered-definitions* [::variable->definition variable]))

(defn get-registered-global-variable [current-ns symbol]
  (let [meta-info (get-in @*registered-definitions* [::symbol->meta symbol])
        private? (get meta-info :private)
        registered-ns (get meta-info :ns)]
    (if (or (not private?)
            (= registered-ns current-ns))
      (get-in @*registered-definitions*
              [::symbol->variable symbol])
      (throw (IllegalArgumentException. (str "Attempting to access private symbol " symbol))))))

(defn print-variable-definition [symbol]
  (meliae.patterns/print-pattern (variable->definition (get-registered-global-variable (namespace symbol) symbol))))

(defn get-global-dependencies [variable]
  (get-in *registered-definitions* [::variable->dependencies variable]))

(defn variable->sorted-dependencies [variable]
  (let [deps (get @*registered-definitions* ::variable->dependencies)
        ;; TODO prevent infinite loops
        f (fn f [root]
            (into []
                  (concat (mapcat f (get deps root))
                          [root])))]
    (f variable)))

(defn symbol->symbols-to-recompile [sym]
  (into #{}
        (let [compiled-symbols (get @*registered-definitions* ::compiled-symbols)]
          (filter (fn [candidate-symbol]
                    (let [candidate (get-registered-global-variable (namespace candidate-symbol) candidate-symbol)
                          candidate-dependencies (->> (variable->sorted-dependencies candidate)
                                                      (map common/variable->original-symbol)
                                                      (into #{}))]
                      (and (not (= sym candidate-symbol))
                           (contains? candidate-dependencies sym))))
                  compiled-symbols))))

(defn build-pre-ast-with-dependencies [variable]
  (let [definition (variable->definition variable)
        substitutions (into {}
                            (for [variable (variable->sorted-dependencies variable)]
                              [variable (common/unqualify-and-duplicate-variable variable "-local")]))
        dependencies (for [variable (variable->sorted-dependencies variable)
                           :let [local-variable (get substitutions variable)]]
                       [local-variable
                        (pre-algorithms/substitute-variables (variable->definition variable)
                                                             substitutions)])
        
        [f-var f-definition] (last dependencies)
        f-source-context (common/variable->source-context f-var)
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
                          (common/unqualify-and-duplicate-variable f-var "-inner")
                          f-arg-vars
                          pre)]
    pre))
