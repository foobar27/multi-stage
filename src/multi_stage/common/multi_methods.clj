(ns multi-stage.common.multi-methods
  (:refer-clojure :exclude [defmulti defmethod remove-method prefer-method get-method methods prefers remove-all-methods])
  (:require [multi-stage.common.hierarchy :as h]))

;; TODO spec that hierarchy is an atom!
(defn make-multi-method [name dispatch-fn hierarchy default-dispatch-value]
  {::name name
   ::dispatch-fn dispatch-fn
   ::hierarchy hierarchy
   ::default-dispatch-value default-dispatch-value
   ::method-table {}
   ::prefer-table {}})

(defn add-method [multi-method dispatch-val method]
  (assoc-in multi-method [::method-table dispatch-val] method))

(defn remove-method [multi-method dispatch-val]
  (update multi-method ::method-table
          (fn [t]
            (dissoc t dispatch-val))))

(defn- prefers? [{:keys [::prefer-table ::hierarchy] :as multi-method} x y]
  (let [x-prefs (get prefer-table x)]
    (or (contains? x-prefs y)
        (let [hierarchy @hierarchy]
          (some #(prefers? multi-method x %) (h/parents hierarchy y))
          (some #(prefers? multi-method % y) (h/parents hierarchy x))))))

(defn- dominates? [multi-method x y]
  (or (prefers? multi-method x y)
      (h/isa? @(::hierarchy multi-method) x y)))

clojure.core/prefer-method

(defn prefer-method [multi-method dispatch-val-x dispatch-val-y]
  (if (prefers? multi-method dispatch-val-y dispatch-val-x)
    (throw (IllegalStateException. (str "Preference conflict in multi-method " (::name multi-method) ": "
                                        dispatch-val-y " is already preferred to " dispatch-val-x)))
    (update-in multi-method
               [::prefer-table dispatch-val-x]
               #(conj (or % #{}) dispatch-val-y))))

(defn prefers [multi-method]
  (get multi-method ::prefer-table))

(defn- determine-best-entry [{:keys [::hierarchy ::method-table] :as multi-method} dispatch-val]
  (let [hierarchy @hierarchy]
    (letfn [(improve [best-entry entry]
              (if (or (nil? best-entry)
                      (dominates? multi-method (first entry) (first best-entry)))
                entry
                best-entry))
            (improve-and-verify [best-entry entry]
              (let [best-entry (improve best-entry entry)]
                (if (dominates? multi-method (first best-entry) (first entry))
                  best-entry
                  ;; TODO do not throw the exception, return it.
                  (throw (IllegalArgumentException. (str "Multiple methods in multimethod " name
                                                         "match dispatch value" dispatch-val "->"
                                                         (first entry) "and" (first best-entry)))))))]
      (reduce improve-and-verify
              nil
              (filter #(h/isa? hierarchy dispatch-val (first %)) method-table)))))

(defn get-method [{:keys [::hierarchy ::method-table ::default-dispatch-value] :as multi-method} dispatch-val]
  (let [best-entry (determine-best-entry multi-method dispatch-val)]
    (if (nil? best-entry)
      (get method-table default-dispatch-value)
      (second best-entry))))

(defn- determine-candidates [hierarchy dispatch-val]
  (into #{dispatch-val}
        (h/descendants hierarchy dispatch-val)))

(comment
  (defn group-methods [multi-method]
    (let [hierarchy @(::hierarchy multi-method)
          method-table (::method-table multi-method)
          candidate-keys (into #{}
                               (mapcat #(determine-candidates hierarchy)
                                       (keys method-table)))
          candidate-map]
      (group-by candidates))))
