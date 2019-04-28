(ns multi-stage.ir.core)

(defn lift [arg]
  (throw (IllegalStateException. "Must be in parsed block.")))

(defn lift-loop [arg]
  (throw (IllegalStateException. "Must be in parsed block.")))

(defn run [arg]
  (throw (IllegalStateException. "Must be in parsed block.")))
