(ns towers.test-utils
  (:require [meliae.patterns :refer [print-pattern]]
            [clojure.test :as t]
            [zprint.core :as zp]
            [clojure.pprint :refer :all]))

(defn- cast-to-symbol [v]
  (cond
    (var? v) (let [n (.ns v)
                   v (.sym v)]
               (symbol (str n) (str v)))
    (symbol? v) v
    (instance? java.lang.Class v) (symbol (.getName v))))

(defn- ns->symbol-map [n]
  (into {} (for [[k v] (ns-map n)]
             [(cast-to-symbol v) (str k)])))

(defn- ns->alias-map [n]
  (into {}
        (for [[s n] (ns-aliases n)]
          [(str n) (str s)])))

;; Within the deftest macro, *ns* points to a different namespace,
;; so we have to name it specifically in these macros.
(def ^:private our-namespace 'towers.test-utils)

(t/deftest symbol-map-examples
  (let [symbol-map (ns->symbol-map our-namespace)]
    (t/are [input expected] 
        (= (get symbol-map input)
           expected)
      `map "map"
      `String "String")))

(defn- ns->ns-context [n]
  {:namespace (name (ns-name n))
   :alias-map (ns->alias-map n)
   :symbol-map (ns->symbol-map n)})

(defn- simplify-qualification [x ns-context]
  (letfn [(same-namespace? [x]
            (let [n (namespace x)]
              (or (nil? n)
                  (= n (:namespace ns-context)))))
          (get-prefix [x]
            (str (get-in ns-context [:alias-map (namespace x)] (namespace x)) "/"))]
    (cond
      ;; TODO unify cases of kewyword and symbol
      (qualified-keyword? x) (if (same-namespace? x)
                               (str "::" (name x))
                               (str ":" (get-prefix x) (name x)))
      ;; TODO needed?
      (keyword? x) (str x)
      (symbol? x) (or (get-in ns-context [:symbol-map x])
                      (if (same-namespace? x)
                        (name x)
                        (str (get-prefix x) (name x)))))))

(t/deftest simple-qualifications-examples
  (let [ctx (ns->ns-context our-namespace)]
    (t/are [input expected] (= (simplify-qualification input ctx)
                               expected)
      `map "map"
      `clojure.core/unknown-symbol "clojure.core/unknown-symbol"
      'unknown-symbol "unknown-symbol"
      `unknown-symbol "unknown-symbol"
      `unknown-ns/unknown-symbol "unknown-ns/unknown-symbol"
      :unqualified ":unqualified"
      ::a "::a"          ;; qualified in local namespace
      ::t/test ":t/test" ;; aliased namespace
      :clojure.test/test ":t/test"
      :unknown-ns/test ":unknown-ns/test"))) 

;; ;; TODO code walking: what if we have (let [map 42] ...)
;; (println `(let [~'map 42]
;;             (println `map)))
;; ;; TODO un-syntax-quote (also gensym, auto-gensym, reader macros & co)

;; (zp/zprint `(defn foo# [x#] (str x# 42 ::x))
;;            {:map {:lift-ns-in-code? true}
;;             :style :community})

(defn refer-private [ns]
  (doseq [[symbol var] (ns-interns ns)]
    (when (:private (meta var))
      (intern *ns* symbol var)))) 

(defmacro verify-pattern [actual expected]
  `(t/is (= (let [code# ~actual]
              (print "verify: ")
              (print-pattern code#)
              (println)
              code#)
            ~expected)))
