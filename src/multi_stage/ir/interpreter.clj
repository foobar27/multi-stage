(ns multi-stage.ir.interpreter
  (:refer-clojure :exclude [reify])
  (:require [clojure.spec.alpha :as s]
            [multi-stage.reflection :refer [invoke-constructor invoke-method]]
            [multi-stage.ir.ast :refer [;; expression constructors
                                        ->literal ->variable ->lambda ->apply ->primitive-call ->let ->if ->do
                                        ->lift ->run ->quote ->throw ->new ->dot
                                        ;; value constructors
                                        ->constant ->closure ->code code?]
             :as ast]
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

(defn conj-block! [s]
  (swap! state update :block conj s))

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
    (ast/constant? e) (->code (->literal (::ast/value e)))
    true (throw (IllegalArgumentException. (str "Neither a code-, nor a constant-expression: " (pattern->string e))))))

;; TODO spec
(defn run [f-lazy]
  (restore-state (f-lazy)))

(defn fold-right [f val c]
  (reduce #(f %2 %1)
          val
          (reverse c)))

(defn- unnamed-let [expression body]
  (->let expression body 'unnamed-let))

;; TODO spec
(defn reify [f-lazy]
  (run #(do (reset-block!)
            (fold-right unnamed-let (f-lazy) (get-block)))))

;; TODO spec
(defn reflect [s]
  (do
    (conj-block! s)
    (fresh!)))

(defn reifyc [f-lazy]
  (reify #(match* [(f-lazy)]
            [(->code e)] e)))

(defn reflectc [s]
  (-> s reflect ->code))

(defn reifyv [f-lazy]
  (run #(do (reset-block!)
            (let [res (f-lazy)]
              (let [b (get-block)]
                (if (empty? b)
                  res
                  (match* [res]
                    [(->code l)] (->code (fold-right unnamed-let l b)))))))))

(declare evalms)

(s/fdef lift
  :args (s/cat :v ::ast/value)
  :ret ::ast/expression)
(defn lift [v]
  (match* [v]
    
    [(->constant n)]
    (->literal n)

    ;; (Rep[A],Rep[B]) ==> Rep[(A,B)]
    ;; [(->tuple (->code uu) (->code vv))]
    ;; (reflect (->cons uu vv))

    ;;  Rep[A]=>Rep[B]  ==> Rep[A=>B]
    [(->closure arity body-env body original-function-name original-argument-names)]
    (-> (->lambda arity
                  (-> #(verify-code
                        (evalms (into (vec body-env)
                                      (repeatedly (inc arity) (fn [] (->code (fresh!)))))
                                body))
                      reifyc)
                  original-function-name
                  original-argument-names)
        reflect) 

    [(->code e)]
    (reflect (->lift e))))

(s/fdef liftc
  :args ::ast/value
  :ret ::ast/expression)
(defn liftc [v]
  (-> v lift ->code))

;;
;; Multi-stage evaluation
;;
(declare evalms)
(declare evalmsg)

(defn- remove-code-lift-constant [arg]
  (cond
    (ast/code? arg)     (::ast/expression arg)
    (ast/constant? arg) (ast/->literal (::ast/value arg))))

(defn- process-arguments [env args evaluate-now build-ast to-string]
  (let [args (map #(evalms env %) args)]
    (cond

      ;; all constants -> evaluate and return constant
      (every? ast/constant? args)
      (->constant (evaluate-now (map ::ast/value args)))

      ;; some code, some constant
      ;; -> get expression from code, wrap constant into code
      ;; -> reflect code block
      (every? (some-fn ast/code? ast/constant?) args)
      (reflectc (build-ast (map remove-code-lift-constant args)))

      ;; else
      true (throw (IllegalArgumentException. (str "Unhandled case:" (to-string args)))))))

(s/fdef evalms
  :args (s/cat :env ::ast/environment
               :e   ::ast/expression)
  :ret ::ast/value)
(defn evalms [env e]
  (do
    (comment (println "evalms " (pattern->string e)))
    (match* [e]
      [(->literal n)]
      (->constant n)

      [(->quote form)]
      (->constant form)
      
      [(->variable n)]
      (nth env n)
      
      [(->lambda arity body original-function-name original-argument-names)]
      (->closure arity env body original-function-name original-argument-names)

      [(->let e1 e2 original-name)]
      (let [v1 (evalms env e1)]
        (evalms (conj (vec env) v1)
                e2))

      [(->do expressions)]
      (last (map #(evalms env %) expressions))
      
      [(->lift ee)]
      (liftc (evalms env ee))

      [(->primitive-call f args)]
      (process-arguments env
                         args
                         (fn evaluate-now [args]
                           (apply (resolve f) args))
                         (fn build-ast [args]
                           (->primitive-call f args))
                         (fn to-string [args]
                           (str "Primitive call to " f
                                " with args " (patterns->string args))))
      
      [(->dot object method-name args)]
      (process-arguments env
                         (into [object] args)
                         (fn evaluate-now [[object & args]]
                           (invoke-method object method-name args))
                         (fn build-ast [[object & args]]
                           (->dot object method-name args))
                         (fn to-string [[object & args]]
                           (str "Call to method " method-name
                                " on object " (pattern->string object)
                                " with args " (patterns->string args))))
      
      [(->throw exception)]
      (process-arguments env
                         [exception]
                         (fn evaluate-now [[exception]]
                           (throw exception))
                         (fn build-ast [[exception]]
                           (->throw exception))
                         (fn to-string [[exception]]
                           ("Throw exception " (pattern->string exception))))
      
      [(->new class-name args)]
      (process-arguments env
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
      (let [function (evalms env function)
            arguments (doall (map #(evalms env %) arguments))]
        (match* [function]

          [(->closure arity body-env body original-function-name original-argument-names)]
          (if (= arity (count arguments))
            (evalms (into (conj (vec body-env)
                                (->closure arity body-env body original-function-name original-argument-names))
                          arguments)
                    body)
            (throw (IllegalArgumentException. (str "Arity mismatch, expected " arity " but got " (count arguments) " arguments for function " (pattern->string function) " arguments: " (patterns->string arguments)))))

          [(->code body)]
          (if (every? (some-fn code? ast/constant?) arguments)
            (reflectc (->apply body (map #(::ast/expression (verify-code-or-lift-constant %)) arguments)))
            (throw (IllegalArgumentException. (str "All non-constant arguments of lifted function application must be lifted, function: " (pattern->string function) " arguments: " (patterns->string arguments)))))))
      
      [(->if condition then else)]
      (match* [(evalms env condition)]
        
        [(->constant n)]
        (if n
          (evalms env then)
          (evalms env else))

        [(->code condition1)]
        (reflectc (->if condition1
                        (reifyc #(verify-code-or-lift-constant (evalms env then)))
                        (reifyc #(verify-code-or-lift-constant (evalms env else))))))

      [(->run b ee)]
      (match* [(evalms env b)]

        [(->code b1)]
        (reflectc (->run b1 (reifyc #(evalms env ee))))

        [_]
        (evalmsg env (reifyc #(do
                                (set-fresh! (count env))
                                (evalms env ee))))))))


(s/fdef evalmsg
  :args (s/cat :env ::ast/environment
               :e ::ast/expression)
  :ret  ::ast/expression)
(defn evalmsg [env e]
  (reifyv #(evalms env e)))
