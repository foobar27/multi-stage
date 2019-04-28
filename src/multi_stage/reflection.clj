(ns multi-stage.reflection
  (:import [clojure.lang Reflector]))

(defn invoke-constructor [clazz args]
  ;; TODO we need to execute boxArgs as in
  ;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Compiler.java#L2615
  (Reflector/invokeConstructor clazz (into-array Object args)))

(defn invoke-method [object method-name args]
  (Reflector/invokeInstanceMethod object (str method-name) (into-array Object args)))
