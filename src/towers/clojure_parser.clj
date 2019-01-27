(ns towers.clojure-parser)

(comment
  (([x] (clojure.core/+ x 1)))
  ([x] (clojure.core/+ x 1)))

(defn destructure-fn* [args]
  (let [;; add optional name (nil)
        [name & args] (if (symbol? (first args))
                        args
                        (into [nil] args))
        ;; add optional parameters for multiple-arity syntax
        arities (if (seq? (first args))
                  args
                  (list args))
        
        ;; parse arities into :args and :body
        arities (for [[args body] arities]
                  {:args args
                   :body [body]}) ;; TODO support implicit do (several statements in body)

        ;; group arities by #args
        arities (into  {} (for [arity arities]
                            (vec [(count (:args arity)) arity])))]
    {:name name
     :arities arities}))
