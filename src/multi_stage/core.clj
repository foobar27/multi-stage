(ns multi-stage.core
  (:refer-clojure :exclude [def defn defmulti defmethod])
  (:require [multi-stage.impl.core :as impl]
            [clojure.spec.alpha :as s]))

(s/fdef def
  :args (s/alt :arity-1 (s/cat :symbol symbol?)
               :arity-2 (s/cat :symbol symbol?
                               :init any?)
               :arity-3 (s/cat :symbol symbol?
                               :docstring string?
                               :init any?))
  :ret any?)
(defmacro def
  "Like clojure.core/def, but to be used in multi-stage programs."
  {:arglists '([symbol docstring? init?])}
  [& args]
  (impl/expand-and-register-def! &form 'def))

(s/fdef defn
  :args :clojure.core.specs.alpha/defn-args
  :ret any?)
(defmacro defn
  "Like clojure.core/defn, but to be used in multi-stage programs."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  [& args]
  (impl/expand-and-register-def! &form 'clojure.core/defn))

(s/fdef defn-
  :args :clojure.core.specs.alpha/defn-args
  :ret any?)
(defmacro defn-
  "Like clojure.core/defn-, but to be used in multi-stage programs."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  [& args]
  (impl/expand-and-register-def! &form 'clojure.core/defn-))
