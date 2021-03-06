(ns multi-stage.ir.interpreter
  (:refer-clojure :exclude [reify])
  (:import [clojure.lang ArityException])
  (:require [clojure.spec.alpha :as s]
            [multi-stage.reflection :refer [invoke-constructor invoke-method]]
            [multi-stage.ir.ast :refer [->literal ->variable ->fn ->apply ->let ->if ->do
                                        ->lift ->run ->throw ->new ->dot ->primitive-symbol
                                        ->literal-set ->literal-vector ->literal-map]
             :as ast]
            [multi-stage.ir.value :refer [->constant ->closure ->code code?] :as value]
            [meliae.patterns :refer [match*]]))

;; TODO move to meliae
(defn pattern->string [pattern]
  (with-out-str (meliae.patterns/print-pattern pattern)))

(defn patterns->string [patterns]
  (pattern->string (vec patterns)))

;; TODO make more fns private


;;
;; Normalization-by-evaluation (NBE)-style polymorphic lift operator
;;

(defrecord State [fresh block])

(def empty-state (->State 0 []))
(def state (atom empty-state))

(defn reset-block! []
  (swap! state assoc :block []))

(defn set-fresh! [i]
  (swap! state assoc :fresh i))

(defn get-block []
  (:block @state))

(defn conj-block! [s name]
  (swap! state update :block conj {::name name, ::expression s}))

(defmacro restore-state [& body]
  `(let [s# @state]
     (try
       ~@body
       (finally
         (reset! state s#)))))

(defn fresh! []
  (let [id (dec (:fresh (swap! state update :fresh inc)))]
    (->variable id)))

(defn- verify-code [e]
  (if (code? e)
    e
    (throw (IllegalArgumentException. (str "Not a code-expression: " (pattern->string e))))))

(defn- verify-code-or-lift-constant [e]
  (cond
    (code? e) e
    (value/constant? e) (->code (->literal (::value/value e)))
    true (throw (IllegalArgumentException. (str "Neither a code-, nor a constant-expression: " (pattern->string e))))))

;; TODO spec
(defn run [f-lazy]
  (restore-state (f-lazy)))

(defn fold-right [f val c]
  (reduce #(f %2 %1)
          val
          (reverse c)))

(defn- named-let-block [{:keys [::name ::expression]} body]
  (->let expression body (or name 'unnamed-let)))

;; TODO spec
(defn reify [f-lazy]
  (run #(do (reset-block!)
            (fold-right named-let-block (f-lazy) (get-block)))))

;; TODO spec
(defn reflect [s name]
  (do
    (conj-block! s name)
    (fresh!)))

(defn reifyc [f-lazy]
  (reify #(match* [(f-lazy)]
            [(->code e)] e)))

(defn reflectc [s name]
  (-> s
      (reflect name)
      ->code))

(defn reifyv [f-lazy]
  (run #(do (reset-block!)
            (let [res (f-lazy)]
              (let [block (get-block)]
                (if (empty? block)
                  res
                  (match* [res]
                    [(->code l)] (->code (fold-right named-let-block l block)))))))))

(declare evalms)

(s/fdef lift
  :args (s/cat :v ::value/value)
  :ret ::ast/expression)
(defn lift [v preferred-symbol]
  (match* [v]
    
    [(->constant n)]
    (->literal n)

    ;; (Rep[A],Rep[B]) ==> Rep[(A,B)]
    ;; [(->tuple (->code uu) (->code vv))]
    ;; (reflect (->cons uu vv))

    ;;  Rep[A]=>Rep[B]  ==> Rep[A=>B]
    [(->closure arity body-env body original-function-symbol original-argument-symbols)]
    (-> (->fn arity
              (-> #(verify-code-or-lift-constant
                    (evalms (into (vec body-env)
                                  (repeatedly (inc arity) (fn [] (->code (fresh!)))))
                            body
                            (or preferred-symbol original-function-symbol)))
                  reifyc)
              original-function-symbol
              original-argument-symbols)
        (reflect (or preferred-symbol
                     original-function-symbol))) 

    [(->code e)]
    (reflect (->lift e)
             preferred-symbol)))

(s/fdef liftc
  :args ::value/value
  :ret ::ast/expression)
(defn liftc [v preferred-symbol]
  (-> v
      (lift preferred-symbol)
      ->code))

;;
;; Multi-stage evaluation
;;
(declare evalms)
(declare evalmsg)

(defn- remove-code-lift-constant [arg]
  (cond
    (value/code? arg)     (::value/expression arg)
    (value/constant? arg) (ast/->literal (::value/value arg))))

(defn- process-arguments [env preferred-symbol args evaluate-now build-ast to-string]
  ;; TODO would be really awesome to take the names of the args from the signature,
  ;; if available! (maybe with arg prefix?)
  (let [args (map #(evalms env % 'unnamed-arg) args)]
    (cond

      ;; all constants -> evaluate and return constant
      (every? value/constant? args)
      (->constant (evaluate-now (map ::value/value args)))

      ;; some code, some constant
      ;; -> get expression from code, wrap constant into code
      ;; -> reflect code block
      (every? (some-fn value/code? value/constant?) args)
      (reflectc (build-ast (map remove-code-lift-constant args))
                preferred-symbol)

      ;; else
      true (throw (IllegalArgumentException. (str "Unhandled case:" (to-string args)))))))

(s/fdef evalms
  :args (s/cat :env ::value/environment
               :e   ::ast/expression
               :preferred-symbol symbol?)
  :ret ::value/value)
(defn evalms [env e preferred-symbol]
  (letfn [(eval-primitive-call [f arguments]
            (process-arguments env
                               preferred-symbol
                               arguments
                               (fn evaluate-now [args]
                                 (apply (resolve f) args))
                               (fn build-ast [args]
                                 (->apply (->primitive-symbol f) args))
                               (fn to-string [args]
                                 (str "Primitive call to " f
                                      " with args " (patterns->string args)))))]
    (comment (println "evalms " (pattern->string e)))
    (match* [e]
      [(->literal n)]
      (->constant n)

      [(->literal-vector elements)]
      (process-arguments env
                         preferred-symbol
                         elements
                         (fn evaluate-now [elements]
                           (vec elements))
                         (fn build-ast [elements]
                           (->literal-vector elements))
                         (fn to-string [elements]
                           (str "Literal vector with elements " (patterns->string elements))))

      [(->literal-set elements)]
      (process-arguments env
                         preferred-symbol
                         elements
                         (fn evaluate-now [elements]
                           (set elements))
                         (fn build-ast [elements]
                           (->literal-set elements))
                         (fn to-string [elements]
                           (str "Literal set with elements " (patterns->string elements))))

      [(->literal-map elements)]
      (process-arguments env
                         preferred-symbol
                         elements
                         (fn evaluate-now [elements]
                           (into {} (map vec elements)))
                         (fn build-ast [elements]
                           (->literal-map elements))
                         (fn to-string [elements]
                           (str "Literal map with elements " (patterns->string elements))))

      [(->variable n)]
      (nth env n)

      [(->primitive-symbol x)]
      (->primitive-symbol x)
      
      [(->fn arity body original-function-symbol original-argument-symbols)]
      (->closure arity env body original-function-symbol original-argument-symbols)

      [(->let e1 e2 original-symbol)]
      (let [v1 (evalms env e1 original-symbol)]
        (evalms (conj (vec env) v1)
                e2
                nil))

      [(->do expressions)]
      (last (map #(evalms env % preferred-symbol) expressions))
      
      [(->lift ee)]
      (liftc (evalms env ee preferred-symbol)
             preferred-symbol)

      [(->dot object method-name args)]
      (process-arguments env
                         preferred-symbol
                         (into [object] args)
                         (fn evaluate-now [[object & args]]
                           (invoke-method object method-name args))
                         (fn build-ast [[object & args]]
                           (->dot object method-name (or args [])))
                         (fn to-string [[object & args]]
                           (str "Call to method " method-name
                                " on object " (pattern->string object)
                                " with args " (patterns->string args))))
      
      [(->throw exception)]
      (process-arguments env
                         preferred-symbol
                         [exception]
                         (fn evaluate-now [[exception]]
                           (throw exception))
                         (fn build-ast [[exception]]
                           (->throw exception))
                         (fn to-string [[exception]]
                           (str "Throw exception " (pattern->string exception))))
      
      [(->new class-name args)]
      (process-arguments env
                         preferred-symbol
                         args
                         (fn evaluate-now [args]
                           (if-let [class (resolve class-name)]
                             (if (class? class)
                               (invoke-constructor class args)
                               (throw (ClassNotFoundException. (str "Could not find class " class ", actual resolved symbol " class "  is of type " (type class)))))
                             (throw (ClassNotFoundException. (str "Could not resolve class name: " class)))))
                         (fn build-ast [args]
                           (->new class-name args))
                         (fn to-string [args]
                           (str "Constructor for " class-name " with arguments " (patterns->string args))))

      [(->apply function arguments)]
      (let [function (evalms env function 'function)
            number-of-arguments (count arguments)]
        (or (match* [function]
              [(->primitive-symbol f)]
              (eval-primitive-call f arguments)
              
              [(->constant x)]
              (cond
                
                (or (keyword? x) (symbol? x))
                ;; (:foo m optional-default) => (get m :foo optional-default)
                (let [[m & arguments] arguments]
                  (eval-primitive-call `get
                                       (into [m (->literal x)] arguments)))

                (set? x)
                (if (= number-of-arguments 1)
                  (evalmsg env
                           (->dot (->literal x) 'get arguments)
                           preferred-symbol)
                  (throw (ArityException. number-of-arguments
                                          (str "Invalid number of arguments for set invocation: " number-of-arguments))))

                (map? x)
                (evalmsg env
                         (->dot (->literal x) 'valAt  arguments)
                         preferred-symbol)
                
                (vector? x)
                (if (= number-of-arguments 1)
                  (eval-primitive-call `nth
                                       (into [(->literal x)] arguments))
                  (throw (ArityException. number-of-arguments
                                          (str "Invalid number of arguments for vector invocation: " number-of-arguments)))))
              
              [(->closure arity body-env body original-function-symbol original-argument-symbols)]
              (let [arguments (doall (map #(evalms env % 'unnamed-arg) arguments))]
                (if (= arity (count arguments))
                  (evalms (into (conj (vec body-env)
                                      (->closure arity body-env body original-function-symbol original-argument-symbols))
                                arguments)
                          body
                          preferred-symbol)
                  (throw (IllegalArgumentException. (str "Arity mismatch, expected " arity " but got " (count arguments) " arguments for function " (pattern->string function) " arguments: " (patterns->string arguments))))))

              [(->code body)]
              (let [arguments (doall (map #(evalms env % 'unnamed-arg) arguments))]
                (if (every? (some-fn code? value/constant?) arguments)
                  (reflectc (->apply body (map #(::value/expression (verify-code-or-lift-constant %)) arguments))
                            preferred-symbol)
                  (throw (IllegalArgumentException. (str "All non-constant arguments of lifted function application must be lifted, function: " (pattern->string function) " arguments: " (patterns->string arguments)))))))))
      
      [(->if condition then else)]
      (match* [(evalms env condition 'if-condition)]
        
        [(->constant n)]
        (if n
          (evalms env then 'if-then)
          (evalms env else 'if-else))

        [(->code condition1)]
        (reflectc (->if condition1
                        (reifyc #(verify-code-or-lift-constant (evalms env then 'if-then)))
                        (reifyc #(verify-code-or-lift-constant (evalms env else 'if-else))))
                  preferred-symbol))

      [(->run b ee)]
      (match* [(evalms env b 'b)]

        [(->code b1)]
        (reflectc (->run b1 (reifyc #(evalms env ee)))
                  preferred-symbol)

        [_]
        (evalmsg env
                 (reifyc #(do
                            (set-fresh! (count env))
                            (evalms env ee preferred-symbol)))
                 preferred-symbol)))))


(s/fdef evalmsg
  :args (s/cat :env ::value/environment
               :e ::ast/expression
               :preferred-symbol symbol?)
  :ret  ::ast/expression)
(defn evalmsg [env e preferred-symbol]
  (reifyv #(evalms env e preferred-symbol)))
