(ns multi-stage.common.multi-methods
  (:refer-clojure :exclude [defmulti defmethod remove-method prefer-method get-method methods prefers remove-all-methods])
  (:require [multi-stage.common.hierarchy :as h]))

(defn make-multi-method [name dispatch-fn default-dispatch-value]
  {::name name
   ::default-dispatch-value default-dispatch-value
   ::method-table {}
   ::prefer-table {}})

(defn add-method [multi-method dispatch-val method]
  (assoc-in multi-method [::method-table dispatch-val] method))

(defn remove-method [multi-method dispatch-val]
  (update multi-method ::method-table
          (fn [t]
            (dissoc t dispatch-val))))

(defn- prefers? [{:keys [::prefer-table] :as multi-method} hierarchy x y]
  (let [x-prefs (get prefer-table x)]
    (or (contains? x-prefs y)
        (some #(prefers? multi-method hierarchy x %) (h/parents hierarchy y))
        (some #(prefers? multi-method hierarchy % y) (h/parents hierarchy x)))))

(defn- dominates? [multi-method hierarchy x y]
  (or (prefers? multi-method hierarchy x y)
      (h/isa? hierarchy x y)))

(defn prefer-method [multi-method hierarchy dispatch-val-x dispatch-val-y]
  (if (prefers? multi-method hierarchy dispatch-val-y dispatch-val-x)
    (throw (IllegalStateException. (str "Preference conflict in multi-method " (::name multi-method) ": "
                                        dispatch-val-y " is already preferred to " dispatch-val-x)))
    (update-in multi-method
               [::prefer-table dispatch-val-x]
               #(conj (or % #{}) dispatch-val-y))))

(defn prefers [multi-method]
  (get multi-method ::prefer-table))

(defn- determine-best-entry [{:keys [::method-table] :as multi-method} hierarchy dispatch-val]
  (letfn [(improve [best-entry entry]
            (if (or (nil? best-entry)
                    (dominates? multi-method hierarchy (first entry) (first best-entry)))
              entry
              best-entry))
          (improve-and-verify [best-entry-or-exception entry]
            (condp = (first best-entry-or-exception)
              ::exception best-entry-or-exception ;; propagate exception
              ::value (let [best-entry (second best-entry-or-exception)
                            best-entry (improve best-entry entry)]
                        (if (dominates? multi-method hierarchy (first best-entry) (first entry))
                          [::value best-entry]
                          [::exception (str "Multiple methods in multimethod " name
                                            "match dispatch value" dispatch-val "->"
                                            (first entry) "and" (first best-entry))]))))]
    (reduce improve-and-verify
            [::value nil]
            (filter #(h/isa? hierarchy dispatch-val (first %)) method-table))))

(defn get-method [{:keys [::method-table ::default-dispatch-value] :as multi-method} hierarchy dispatch-val]
  (let [value-or-exception (determine-best-entry multi-method hierarchy dispatch-val)]
    (if (= ::value (first value-or-exception))
      (let [best-entry (second value-or-exception)]
        (if (nil? best-entry)
          (get method-table default-dispatch-value)
          (second best-entry)))
      (throw (IllegalArgumentException. (second value-or-exception))))))

(defn- determine-candidates [hierarchy dispatch-val]
  (into #{dispatch-val}
        (h/descendants hierarchy dispatch-val)))

(comment
  (defn group-methods [multi-method hierarchy]
    (let [method-table (::method-table multi-method)
          candidate-keys (into #{}
                               (mapcat #(determine-candidates hierarchy)
                                       (keys method-table)))
          candidate-map]
      (group-by candidates))))
