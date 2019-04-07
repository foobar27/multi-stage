(ns towers.clojure.parser
  (:require [towers.clojure.ast :refer [smart-do smart-let* smart-fn* smart-invoke smart-literal]]))

(defmulti destructure-clj (fn [sym args] sym))

(defmethod destructure-clj :default [_ args]
  nil)

(defmethod destructure-clj 'let* [_ args]
  (let [[bindings & bodies] args]
    {:bindings (partition 2 bindings)
     :bodies bodies}))

(defmethod destructure-clj 'do [_ args]
  {:bodies args})

(defmethod destructure-clj 'if [_ args]
  (let [[condition then else] args]
    {:condition condition
     :then then
     ;; else is nil by default. This is expected.
     :else else}))

(defmethod destructure-clj 'recur [_ arguments]
  {:arguments arguments})

(defmethod destructure-clj '. [_ args]
  args)

(defmethod destructure-clj 'new [_ [class-name & args]]
  {:class-name class-name
   :arguments args})

(defmethod destructure-clj 'throw [_ args]
  {:exception args})

(defmethod destructure-clj 'loop* [_ [bindings & bodies]]
  {:bindings (doall (map vec (partition 2 bindings)))
   :bodies bodies})

(comment ;; examples for fn*
  (([x] (clojure.core/+ x 1)))
  ([x] (clojure.core/+ x 1)))

(defmethod destructure-clj 'fn* [_ args]
  (let [;; add optional name (nil)
        [name & args] (if (symbol? (first args))
                        args
                        (into [nil] args))
        ;; add optional parameters for multiple-arity syntax
        arities (if (seq? (first args))
                  args
                  (list args))
        
        ;; parse arities into :args and :body
        arities (for [[args & bodies] arities]
                  {:args args
                   :bodies bodies})

        ;; group arities by #args
        arities (into  {} (for [arity arities]
                            (vec [(count (:args arity)) arity])))]
    {:name name
     :arities arities}))

