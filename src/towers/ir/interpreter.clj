(ns towers.ir.interpreter
  (:refer-clojure :exclude [reify])
  (:require [clojure.spec.alpha :as s]
            [towers.ir.ast :refer [;; expression constructors
                                   ->literal ->variable ->lambda ->apply ->primitive-call ->let ->if
                                   ->lift ->run ->quote
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
    (->variable id "fresh")))

(defn- verify-code [e]
  (if (code? e)
    e
    (throw (IllegalArgumentException. (str "Not a code-expression: " e)))))

;; TODO spec
(defn run [f-lazy]
  (restore-state (f-lazy)))

(defn fold-right [f val c]
  (reduce #(f %2 %1)
          val
          (reverse c)))

;; TODO spec
(defn reify [f-lazy]
  (run #(do (reset-block!)
            (fold-right ->let (f-lazy) (get-block)))))

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
                    [(->code l)] (->code (fold-right ->let l b)))))))))

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
    [(->closure env2 e2)]
    (-> #(verify-code
          (evalms (conj (vec env2)
                        (->code (fresh!))
                        (->code (fresh!)))
                  e2))
        reifyc
        ->lambda
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
(s/fdef evalms
  :args (s/cat :env ::ast/environment
               :e   ::ast/expression)
  :ret ::ast/value)
(defn evalms [env e]
  (do
    (print "evalms ")
    (meliae.patterns/print-pattern e)
    (println)
    (match* [e]
      [(->literal n)]
      (->constant n)

      [(->quote form)]
      (->constant form)
      
      [(->variable n s)]
      (nth env n)

      [(->lambda ee)]
      (->closure env ee)

      [(->let e1 e2)]
      (let [v1 (evalms env e1)]
        (evalms (conj (vec env) v1)
                e2))

      [(->lift ee)]
      (liftc (evalms env ee))

      [(->primitive-call f args)]
      (let [args (doall (map #(evalms env %) args))]
        (println "PRIMITIVE CALL TO" f "WITH ARGS" args)
        (cond
          
          (every? ast/constant? args)
          (let [f (resolve f)
                args (map ::ast/value args)]
            (->constant (apply f args)))

          (every? ast/code? args)
          (let [code-args (map ::ast/expression args)]
            (reflectc (->primitive-call f code-args)))

          ;; else
          true (throw (IllegalArgumentException. (str "Unhandled case " f " with args" args)))))
      
      [(->apply e1 [e2])]
      (match* [(evalms env e1) (evalms env e2)]

        [(->closure env3 e3) v2]
        (evalms (conj (vec env3) (->closure env3 e3) v2)
                e3)

        [(->code s1) (->code s2)]
        (reflectc (->apply s1 s2)))
      
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
