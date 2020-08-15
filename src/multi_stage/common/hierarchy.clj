(ns multi-stage.common.hierarchy
  (:refer-clojure :exclude [isa? derive parents ancestors descendants make-hierarchy]))

;; Important note: This hierarchy is different from the clojure
;; hierarchy in the sense that we do not support class inheritance.

(def empty-hierarchy
  {::parents {}
   ::descendants {}
   ::ancestors {}})

(defn make-hierarchy []
  empty-hierarchy)

(defn- add [xs x]
  (conj (or xs #{}) x))

(defn- add-links [input to-be-updated new-items]
  (reduce (fn [output item]
            (update output
                    item
                    #(into (or % #{})
                           new-items)))
          input
          to-be-updated))

(defn derive
  [hierarchy child parent]
  (assert (not= child parent))
  (let [{:keys [::parents ::ancestors ::descendants]} hierarchy]
    (cond
      ;; nothing to do if derivation already exists
      (contains? (get parents child #{}) parent)
      hierarchy

      ;; Fail if parent is already an ancestor of child (but not the parent itself)
      (contains? (get ancestors child #{}) parent)
      (throw (Exception. (str child " already has " parent " as an ancestor.")))

      ;; Fail if we detect a cycle
      (contains? (get ancestors parent #{}) child)
      (throw (Exception. (str "Cyclic derivation: " parent " has " child " as an ancestor.")))

      ;; else
      true
      (let [merge-links (fn [xs x]
                          (conj (get xs x #{}) x))
            all-ancestors   (merge-links ancestors parent)   ;; parent and all its ancestors
            all-descendants (merge-links descendants child)] ;; child and all its descendants
        {::parents     (update parents child #(add % parent))
         ;; Add all-ancestors to all-descendants's ancestors.
         ::ancestors   (add-links ancestors   all-descendants all-ancestors)
         ;; Add all-descendants to all-ancestors's descendants.
         ::descendants (add-links descendants all-ancestors   all-descendants)}))))

(defn parents
  [hierarchy tag]
  (get-in hierarchy [::parents tag]))

(defn ancestors
  [hierarchy tag]
  (get-in hierarchy [::ancestors tag]))

(defn descendants
  [hierarchy tag]
  (get-in hierarchy [::descendants tag]))

(defn isa?
  [hierarchy child parent]
  (or (= child parent)
      (contains? (ancestors hierarchy child) parent)
      (and (vector? parent) (vector? child)
           (= (count parent) (count child))
           (loop [ret true
                  i 0]
             (if (or (not ret) (= i (count parent)))
               ret
               (recur (isa? hierarchy (child i) (parent i)) (inc i)))))))
