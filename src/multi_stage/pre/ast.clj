(ns multi-stage.pre.ast
  (:refer-clojure :exclude [fn?])
  (:require [clojure.spec.alpha :as s]
            [multi-stage.utils :refer [unqualified-symbol?]]
            [multi-stage.common.core :as common]
            [meliae.patterns :refer [defmultipattern defpatterns]]))

;; A pre-AST is a representation of a clojure sexp which is pretty close
;; to the original clojure code. There are however some simplications:
;; - Variable references are resolved and made unique via gensym
;;   to avoid shadowing issues.
;; - We just support special forms. More complex forms (e.g. when-statements)
;;   should be expanded in a previous step via macro-expansion.
;; - We don't support destructuring, we suppose that previous macro-expansions
;;   already took care of that step. 
;; - We don't support multiple-argument let-statements. These need to be replaced
;;   by nested let-satements (or a do-statement in case of zero arguments).
;; - We don't support implicit do statements.
;; - If-statements always have two branches.
;; - loop*/recur is not supported and should be replaced by function application.
;;   The code generation will take care of tail call elimination.
;; - Functions just support a single arity. If you need more arities, you should
;;   create different functions.

;; Nodes will all have a source-context to improve debugging.

;; Any clojure expression will be represented by an individual node in
;; the AST.

;; TODO where shall we replace symbol to variable?
(defmultipattern expression)
(defpatterns expression
  ;; Literals are constants in the clojure code.
  literal            [source-context ::common/source-context
                      value any?]
  ;; Vectors are special cases of literals, because they have a
  ;; constant amount of elements, but the elements can be dynamic
  ;; expressions.
  literal-vector     [source-context ::common/source-context
                      elements (s/coll-of ::expression)]
  ;; Sets are special cases of literals, like vectors.
  literal-set        [source-context ::common/source-context
                      elements (s/coll-of ::expression)]
  ;; Maps are special cases of literals, like vectors.
  ;; Both keys and values can be dynamic expressions.
  literal-map        [source-context ::common/source-context
                      elements (s/coll-of (s/tuple ::expression ::expression))]
  ;; Function definitions are represented by their (unique) name,
  ;; arguments and body. The name will be used to identify recursive
  ;; call sites. We suppose the destructuring has already been done,
  ;; so we just support a linear list of arguments.
  ;; We also just support a single body (see note on implicit-do).
  fn                 [source-context ::common/source-context
                      name ::common/variable
                      arguments (s/coll-of ::common/variable)
                      body ::expression]
  ;; A call site of a function. The expression can be an anonymous
  ;; lambda, or a variable reference to a previously defined function.
  
  ;; TODO what about primitive function calls?
  apply              [source-context ::common/source-context
                      function ::expression
                      arguments (s/coll-of ::expression)]

  ;; A do-block refers to zero or more expressions.
  do                 [source-context ::common/source-context
                      bodies (s/coll-of ::expression)]
  ;; A let-statement consists of a key, a value and a body. Please
  ;; note that we don't support multiple key-values pairs, they will
  ;; be replaced during the parsing step into multiple let statements.
  ;; We also suppose that destructuring has already been done,
  ;; and we just support a single body (see note on implicit-do).
  let                [source-context ::common/source-context
                      key ::common/variable
                      value ::expression
                      body ::expression]
  if                 [source-context ::common/source-context
                      condition ::expression
                      then ::expression
                      else ::expression]
  ;; A dot-expression is a call to a java instance. The instance is
  ;; referred to by the expression which generates the instance, or by
  ;; a references to a variable which points to the instance.
  dot                [source-context ::common/source-context
                      object ::expression
                      method-name unqualified-symbol?
                      arguments (s/coll-of ::expression)]
  class-reference    [source-context ::common/source-context
                      name symbol?]
  variable-reference [;; The source-context where the variable is referenced
                      ;; (might be different from the source context
                      ;; where the variable was defined)
                      source-context ::common/source-context
                      variable ::common/variable]
  symbol-reference   [source-context ::common/source-context
                      symbol qualified-symbol?]
  new                [source-context ::common/source-context
                      class-name symbol?
                      arguments (s/coll-of ::expression)]
  throw              [source-context ::common/source-context
                      exception ::expression])
