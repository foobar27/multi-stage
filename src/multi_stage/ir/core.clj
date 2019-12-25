(ns multi-stage.ir.core
  (:require [multi-stage.common.core :refer [mockable-gensym]]))

(defn lift [arg]
  (throw (IllegalStateException. "Must be in parsed block.")))

(comment  
  (loop [x 1]
    (f x))

  =>
  
  ((fn [x]
     )
   1))


(defn- generate-lifted-loop* [bindings & bodies]
  (let [bindings (partition 2 bindings)
        binding-keys (map first bindings)
        binding-values (map second bindings)
        argument-symbols (for [[k v] bindings]
                           (mockable-gensym k))]
    `(let [~@(mapcat (fn [sym value]
                       [sym value])
                     argument-symbols
                     binding-values)]
       ((lift (fn [~@binding-keys]
                ~@bodies))
        ~@argument-symbols))))

(defmacro lift-loop [bindings & bodies]
  (let [expanded (macroexpand-1 `(loop ~bindings ~@bodies))
        throw-unrecognized (fn []
                             (throw (IllegalArgumentException.
                                     (str "Did not recognize expanded loop: " expanded))))]
    (condp = (first expanded)
      'loop* (let [[_ loop-bindings & loop-bodies] expanded]
               (apply generate-lifted-loop* loop-bindings loop-bodies))
      `let (let [[_ let-bindings & let-bodies] expanded]
             (condp = (count let-bodies)
               1 (let [[loop-statement loop-bindings & loop-bodies] let-bodies]
                   (if (= loop-statement 'loop*)
                     `(let [~@let-bindings]
                        ~(apply generate-lifted-loop* loop-bindings loop-bodies))
                     (throw-unrecognized)))
               (throw-unrecognized)))
      (throw-unrecognized))))

(defn run [arg]
  (throw (IllegalStateException. "Must be in parsed block.")))
