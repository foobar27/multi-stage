(ns towers.reflection
  (:import [clojure.lang Reflector]))

(defn invoke-constructor [clazz args]
  ;; TODO we need to execute boxArgs as in
  ;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Compiler.java#L2615
  (Reflector/invokeConstructor clazz (into-array Object args)))
