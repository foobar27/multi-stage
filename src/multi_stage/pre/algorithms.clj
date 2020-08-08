(ns multi-stage.pre.algorithms
  (:require [multi-stage.pre.ast :as ast]
            [meliae.patterns :refer [match*]]))

(defn substitute-variables [e replacement-map]
  (let [recur-item (fn [item]
                     (substitute-variables item replacement-map))
        recur-items (fn [items]
                      (map recur-item items))
        replace (fn [variable]
                  (or (get replacement-map variable)
                      variable))]
    (match* [e]
      
      [(ast/->literal source-context value)]
      (ast/->literal source-context value)

      [(ast/->literal-vector source-context elements)]
      (ast/->literal-vector source-context (recur-items elements))

      [(ast/->literal-set source-context elements)]
      (ast/->literal-set source-context (recur-items elements))

      [(ast/->literal-map source-context elements)]
      (ast/->literal-map source-context (for [[k v] elements]
                                          [k (recur-item v)]))

      [(ast/->fn source-context name arguments body)]
      (ast/->fn source-context
                (replace name)
                (map replace arguments)
                (recur-item body))
      
      [(ast/->apply source-context function arguments)]
      (ast/->apply source-context
                   (recur-item function)
                   (recur-items arguments))

      [(ast/->do source-context bodies)]
      (ast/->do source-context
                (recur-items bodies))

      [(ast/->let source-context key value body)]
      (ast/->let source-context
                 (replace key)
                 (recur-item value)
                 (recur-item body))

      [(ast/->if source-context condition then else)]
      (ast/->if source-context
                (recur-item condition)
                (recur-item then)
                (recur-item else))

      [(ast/->dot source-context object method-name arguments)]
      (ast/->dot source-context
                 (recur-item object)
                 method-name
                 (recur-item arguments))

      [(ast/->class-reference source-context name)]
      (ast/->class-reference source-context name)

      [(ast/->variable-reference source-context variable)]
      (ast/->variable-reference source-context (replace variable))
      
      [(ast/->symbol-reference source-context symbol)]
      (ast/->symbol-reference source-context symbol)
      
      [(ast/->new source-context class-name arguments)]
      (ast/->new source-context class-name (recur-items arguments))
      
      [(ast/->throw source-context exception)]
      (ast/->throw source-context (recur-item exception)))))
