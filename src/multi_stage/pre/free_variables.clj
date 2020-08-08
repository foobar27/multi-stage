(ns multi-stage.pre.free-variables
  (:require [multi-stage.pre.ast :as ast]
            [meliae.patterns :refer [match*]]))

(defn- pre->free-global-variables-impl [e all-global-variables]
  (let [recur-item (fn [item]
                     (pre->free-global-variables-impl item all-global-variables))
        recur-items (fn [items]
                      (mapcat recur-item items))]
    (match* [e]
      
      [(ast/->literal source-context value)]
      #{}

      [(ast/->literal-vector source-context elements)]
      (recur-items elements)

      [(ast/->literal-set source-context elements)]
      (recur-items elements)

      [(ast/->literal-map source-context elements)]
      ;; Flatten the keys and values from the map.
      (recur-items (apply concat elements))

      [(ast/->fn source-context name arguments body)]
      (recur-item body)

      [(ast/->apply source-context function arguments)]
      (concat (recur-item function)
              (recur-items arguments))

      [(ast/->do source-context bodies)]
      (recur-items bodies)

      [(ast/->let source-context key value body)]
      (concat (recur-item value)
              (recur-item body))


      [(ast/->if source-context condition then else)]
      (recur-items [condition then else])

      [(ast/->dot source-context object method-name arguments)]
      (concat (recur-item object)
              (recur-items arguments))

      [(ast/->class-reference source-context name)]
      #{}

      [(ast/->variable-reference source-context variable)]
      (do
        (if (contains? all-global-variables variable)
          #{variable}
          #{}))

      [(ast/->symbol-reference source-context symbol)]
      #{}

      [(ast/->new source-context class-name arguments)]
      (recur-items arguments)

      [(ast/->throw source-context exception)]
      (recur-item exception))))

(defn pre->free-global-variables [e all-global-variables]
  (into #{}
        (pre->free-global-variables-impl e (into #{} all-global-variables))))
