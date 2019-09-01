(ns multi-stage.impl.core
  (:require [multi-stage.utils :refer [make-local-symbol]]
            [clojure.spec.alpha :as s]))

(defonce ^:dynamic *registered-definitions* (atom {}))

(defmacro with-clean-definitions [& bodies]
  `(with-bindings {#'*registered-definitions* (atom {})}
     ~@bodies))

(defn reset-definitions []
  (reset! *registered-definitions* {}))

(defn register-definition! [name value]
  (let [key (make-local-symbol name)]
    (swap! *registered-definitions* assoc key {:key key
                                               :value value})))

(s/fdef get-definition-kv
  :args (s/cat :current-ns #(instance? clojure.lang.Namespace %)
               :qualified-symbol qualified-symbol?)
  :ret (s/tuple qualified-symbol? any?))
(defn get-definition-kv
  "Gets a previously registered definition, as a pair of key and value.
  The key is the name, with the original meta information.
  The value is the original form."
  [current-ns qualified-symbol]
  (let [snapshot @*registered-definitions*]
    (println "LOOK UP  " qualified-symbol "IN" snapshot "EQUALS" (get snapshot qualified-symbol))
    (if-let [{:keys [key value]} (get snapshot qualified-symbol)]
      (let [private? (get (meta key) :private)
            registered-ns (get (meta key) :ns)]
        (if (and (not private?)
                 (= registered-ns (ns qualified-symbol)))
          [key value]
          (throw (IllegalArgumentException. (str "Attempting to access private symbol " qualified-symbol))))))))

(defn expand-and-register-def!
  "Abuse macro-expansion of defn by replacing ms-defn with def.
  Be careful to keep the meta data of the original &form.
  replacement should be a macro which expands to a def-form"
  [form replacement]
  (let [form (with-meta (macroexpand-1 `(~replacement ~@(rest form)))
               (meta form))
        ;; TODO missing in the following destructuring: docstring
        [_ name value] form]
    (register-definition! name value)
    nil))
