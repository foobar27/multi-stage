(ns multi-stage.common.core
  (:require [clojure.spec.alpha :as s]
            [multi-stage.utils :refer [unqualified-symbol?]]))

;; == Symbol generation

;; When we want to generaqte a symbol which is guaranteed to be
;; unique, we would usually call gensym. However, this is pretty
;; difficult to test. That's why we have our own variant of gensym,
;; which can be reset during tests.

(def gensym-state (atom nil))

(defn mockable-gensym [prefix-string]
  (if-let [state @gensym-state]
    (let [next-id (get (swap! gensym-state
                              (fn [state]
                                (assoc state
                                       prefix-string
                                       (inc (or (get state prefix-string) -1)))))
                       prefix-string)]
      (symbol (str "MOCKED-" prefix-string next-id)))
    (gensym prefix-string)))

(defmacro mock-gensyms [& bodies]
  `(try
     (reset! gensym-state {})
     ~@bodies
     (finally (reset! gensym-state nil))))

;; == Source Context

;; A source context is a data structures which groups information
;; which will be used for debugging information. It should be carried
;; over during each transformation of the AST to improve formatting of
;; error message, and to make the generated code more readable.

(s/def ::file-name string?)
(s/def ::line-number integer?)
(s/def ::column-number integer?)
(s/def ::source-context
  (s/keys :req [::file-name ::line-number ::column-number]))
(s/fdef ->source-context
  :args (s/cat :file-name ::file-name
               :line-number ::line-number
               :column-number ::column-number)
  :ret ::source-context)
(defn ->source-context [file-name line-number column-number]
  {::file-name file-name
   ::line-number line-number
   ::column-number column-number})

(defn source-context->file-name [v]
  (get v ::file-name))

(defn source-context->line-number [v]
  (get v ::line-number))

(defn source-context->column-number [v]
  (get v ::column-number))

;; == Variables

;; Technically a variable just needs to be a unique object.  In order
;; to improve readability of the generated code, we define a data
;; structure which consists of the original name, a generated unique
;; name (to make the data structure unique), and the source-context
;; where the variable was defined.

(s/def ::variable
  (s/keys :req [::unique-name unqualified-symbol?
                ::original-symbol symbol?
                ::source-context ::source-context]))
(s/fdef ->variable
  :args (s/cat :unique-name unqualified-symbol?
               :original-symbol unqualified-symbol?
               :source-context ::source-context)
  :ret ::variable)
(defn ->variable [unique-name original-symbol source-context]
  {::unique-name unique-name
   ::original-symbol original-symbol
   ::source-context source-context})

(defn variable->unique-name [v]
  (get v ::unique-name))

(defn variable->original-symbol [v]
  (get v ::original-symbol))

(defn variable->source-context [v]
  (get v ::source-context))
