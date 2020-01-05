(ns multi-stage.impl.core
  (:require [multi-stage.utils :refer [make-local-symbol]]
            [multi-stage.common.core :as common :refer [sexp->source-context]]
            [multi-stage.common.core :refer [->variable]]
            [multi-stage.pre.free-variables :refer [pre->free-global-variables]]
            [clojure.spec.alpha :as s]))

;; # Handling of definitions

(def ^:private empty-definitions
  {::symbol->variable {}
   ::variable->definition {}
   ::variable->dependencies {}
   ::symbol->meta {}})

(defonce ^:dynamic *registered-definitions* (atom empty-definitions))

(comment
  (println (::variable->dependencies @*registered-definitions*))

  (reset-definitions!)
  )

(defn determine-all-registered-variables []
  (vals (get @*registered-definitions* ::symbol->variable)))

(defmacro with-clean-definitions [& bodies]
  `(with-bindings {#'*registered-definitions* (atom empty-definitions)}
     ~@bodies))

(defn reset-definitions! []
  (reset! *registered-definitions* empty-definitions))

(defn register-definition! [variable value dependencies]
  (swap! *registered-definitions*
         assoc-in
         [::variable->definition variable]
         value)
  (swap! *registered-definitions*
         assoc-in
         [::variable->dependencies variable]
         dependencies))

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

(defn get-global-dependencies [variable]
  (get-in *registered-definitions* [::variable->dependencies variable]))

(defn create-variable! [symbol source-context]
  (let [variable (->variable (gensym (name symbol)) ;; strip ns part
                             symbol
                             source-context)]
    (swap! *registered-definitions* assoc-in
           [::symbol->variable symbol]
           variable)
    (swap! *registered-definitions* assoc-in
           [::symbol->meta (meta symbol)]
           variable)
    variable))

(defn variable->sorted-dependencies [variable]
  (let [deps (get @*registered-definitions* ::variable->dependencies)
        ;; TODO prevent infinite loops
        f (fn f [root]
            (into []
                  (concat (mapcat f (get deps root))
                          [root])))]
    (f variable)))
