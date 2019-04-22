(ns towers.ir.interpreter
  (:refer-clojure :exclude [reify])
  (:require [clojure.spec.alpha :as s]
            [towers.reflection :refer [invoke-constructor invoke-method]]
            [towers.ir.ast :refer [;; expression constructors
                                   ->literal ->variable ->lambda ->apply ->primitive-call ->let ->if ->do
                                   ->lift ->run ->quote ->throw ->new ->dot
                                   ;; value constructors
                                   ->constant ->closure ->code code?]
             :as ast]
            [meliae.patterns :refer [match*]]))

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

;; TODO spec
(defn run [f-lazy]
  (restore-state (f-lazy)))

(defn fold-right [f val c]
  (reduce #(f %2 %1)
          val
          (reverse c)))

(defn- unnamed-let [expression body]
  (->let expression body "unnamed-let"))

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

;; TODO move to meliae
(defn pattern->string [pattern]
  (with-out-str (meliae.patterns/print-pattern pattern)))

(defn patterns->string [patterns]
  (pattern->string (vec patterns)))

;;
;; Multi-stage evaluation
;;
(declare evalms)
(declare evalmsg)
(s/fdef evalms
  :args (s/cat :env ::ast/environment
               :e   ::ast/expression)
  :ret ::ast/value)
(defn evalms [env e]
  (do
    (println "evalms " (pattern->string e))
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
      (let [args (doall (map #(evalms env %) args))]
        (cond
          
          (every? ast/constant? args)
          (let [f (resolve f)
                args (map ::ast/value args)]
            (->constant (apply f args)))

          (every? code? args)
          (let [code-args (map ::ast/expression args)]
            (reflectc (->primitive-call f code-args)))

          ;; else
          true (throw (IllegalArgumentException. (str "Unhandled case " f " with args " (patterns->string args))))))

      [(->throw exception)]
      (let [exception (evalms env exception)]
        (cond

          (ast/constant? exception)
          (throw (:ast/value exception))

          (ast/code? exception)
          (let [exception (::ast/expression exception)]
            (reflectc (->throw exception)))

          ;; else
          true (throw (IllegalArgumentException. (str "Unhandled case throw " (patterns->string exception))))))

      [(->new class-name args)]
      (let [args (map #(evalms env %) args)]
        (cond

          (every? ast/constant? args)
          (if-let [class (resolve class-name)]
            (if (class? class)
              (let [args (map ::ast/value args)]
                (->constant (invoke-constructor class args)))
              (throw (ClassNotFoundException. (str "Could not resolve class name: " class ", actual resolved value " class))))
            (throw (ClassNotFoundException. (str "Could not resolve class name: " class))))

          (every? ast/code? args)
          (let [code-args (map ::ast/expression args)]
            (reflectc (->new class-name code-args)))

          ;; else
          true (throw (IllegalArgumentException. (str "Unhandled case " class-name " with args " (patterns->string args))))))

      [(->dot object method-name args)]
      (let [object (evalms env object)
            args (map #(evalms env %) args)]
        (cond

          (and (ast/constant? object)
               (every? ast/constant? args))
          (let [object (::ast/value object)
                args (map ::ast/value args)]
            (invoke-method object method-name args))

          (and (ast/code? object)
               (every? ast/code? args))
          (let [object (::ast/expression object)
                code-args (map ::ast/expression args)]
            (reflectc (->dot object method-name code-args)))

          ;; else
          true (throw (IllegalArgumentException. (str "Unhandled case " (pattern->string object) "." method-name " with args " (patterns->string args))))))
      
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
          (if (every? code? arguments)
            (reflectc (->apply body arguments))
            (throw (IllegalArgumentException. (str "All arguments of lifted function application must be lifted, function: " (pattern->string function) " arguments: " (patterns->string arguments)))))))
      
      [(->if condition then else)]
      (match* [(evalms env condition)]
        
        [(->constant n)]
        (if n
          (evalms env then)
          (evalms env else))

        [(->code condition1)]
        (reflectc (->if condition1
                        (reifyc #(verify-code (evalms env then)))
                        (reifyc #(verify-code (evalms env else))))))

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
