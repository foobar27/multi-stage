(ns multi-stage.ir.parser
  (:require [multi-stage.ir.ast :refer [->literal ->variable ->do ->let ->fn ->apply ->dot ->new
                                        ->if ->lift ->run ->primitive-call ->throw ->class-reference
                                        ->primitive-symbol ->literal-set ->literal-vector ->literal-map]
             :as ast]
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [macroexpand-all]]
            [meliae.patterns :refer [match*]]
            [multi-stage.pre.ast :as pre-ast]
            [multi-stage.clojure.parser :refer [destructure-clj]]
            [multi-stage.ir.core :refer [lift run]]
            [multi-stage.common.core :as common]))

;; == Variable Handling
;;
;; Variables are organized via de Bruijn indices. Although the
;; datastructure is essentially a stack, we implement it via a map
;; where the keys are variables (as in the pre-AST) and the values are
;; the index ont the stack.
;;
;; Please note that variable shading cannot occur, because the pre-AST
;; already took care of that.

(s/def ::scope
  (s/map-of ::common/variable integer?))

(s/fdef push-var
  :args (s/cat :scope ::scope
               :ret ::common/variable)
  :ret ::scope)
(defn- push-var
  "Declare a new variable, pushes it onto the stack and returns the new
  stack."
  [scope var]
  (assoc scope
         var
         (if-let [xs (vals scope)]
           (inc (apply max xs))
           0)))

(defn- push-vars
  [scope vars]
  (reduce push-var scope vars))

(s/fdef get-var-index
  :args (s/cat :scope ::scope
               :ret ::common/variable)
  :ret integer?)
(defn get-var-index
  "Gets the index of an existing variable. Throws an exception if the
  variable is unknown."
  [scope var]
  (println "SCOPE" scope)
  (or (get scope var)
      (throw (IllegalArgumentException. (str "Unknown variable: " var)))))

;; == Pre-defined symbols
;;
;; We maintain a whitelist of symbols which can be safely executed at
;; compile-time.

(def primitive-fns
  #{`get `seq `seq? `chunked-seq? `first `chunk-first `rest `chunk-rest `next `nth
    `set `map `vec
    `assoc! `conj! `transient `persistent! `cons `concat `list
    `+ `- `* `/ `inc `dec `pos?
    `number? `symbol?
    `= `< `> `int `long `str `count
    `unchecked-inc `unchecked-add `unchecked-add-int `unchecked-byte
    `unchecked-char `unchecked-dec `unchecked-dec-int `unchecked-divide-int
    `unchecked-double `unchecked-float})

;; Additionally, we have some special symbols which will have
;; dedicated nodes in the syntax tree

(def special-fn-map
  {`lift ->lift,
   `run ->run})

;; == Parsing: Translate pre-ast to ir-ast

(declare pre->ir)

(defn pres->irs [exps scope]
  (map #(pre->ir % scope) exps))

(defn pre->ir
  "Translate a pre-ast into an ir-ast.
  - replace symbols by de Bruijn indices
  - replace primitive function calls by specific ir-ast node"
  [e scope]
  (let [recur-item (fn [e]
                     (pre->ir e scope))
        recur-items (fn [es]
                      (pres->irs es scope))]
    (match* [e]
      
      [(pre-ast/->literal source-context value)]
      (->literal value)

      [(pre-ast/->literal-vector source-context elements)]
      (->literal-vector (recur-items elements))

      [(pre-ast/->literal-set source-context elements)]
      (->literal-set (recur-items elements))

      [(pre-ast/->literal-map source-context elements)]
      (->literal-map (for [[k v] elements]
                       [(recur-item k) (recur-item v)]))

      [(pre-ast/->variable-reference source-context reference)]
      (->variable (get-var-index scope reference))
      
      [(pre-ast/->symbol-reference source-context reference)]      
      (if (contains? primitive-fns reference)
        (->primitive-symbol reference)
        (throw (IllegalArgumentException. (str "Unable to translate symbol reference, maybe a missing entry in primitive-fns for " reference))))
      
      [(pre-ast/->class-reference source-context reference)]
      (->class-reference reference)

      [(pre-ast/->do source-context bodies)]
      (->do (recur-items bodies))

      [(pre-ast/->let source-context k v body)]
      (let [v (recur-item v)
            scope (push-var scope k)]
        (->let v
               (pre->ir body scope)
               (::common/original-symbol k)))

      [(pre-ast/->apply source-context function arguments)]
      (let [arguments (recur-items arguments)]
        (match* [function]
          [(pre-ast/->symbol-reference source-context2 symbol)]
          (if-let [ctor (get special-fn-map symbol)]
            (apply ctor arguments)
            (->apply (recur-item function) arguments))
          :else (->apply (recur-item function) arguments)))
      
      [(pre-ast/->fn source-context name args body)]
      (->fn (count args)
            (pre->ir body (push-vars (push-var scope name) args))
            (::common/original-symbol name)
            (map ::common/original-symbol args))

      [(pre-ast/->if source-context condition then else)]
      (->if (recur-item condition)
            (recur-item then)
            (recur-item else))

      [(pre-ast/->dot source-context object method-name arguments)]
      (->dot object method-name (recur-items arguments))

      [(pre-ast/->new source-context class-name arguments)]
      (->new class-name (recur-items arguments))

      [(pre-ast/->throw source-context exception)]
      (->throw (recur-item exception)))))
