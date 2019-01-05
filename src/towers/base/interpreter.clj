(ns towers.base.interpreter
  (:refer-clojure :exclude [reify])
  (:require [clojure.spec.alpha :as s]
            [towers.base.ast :refer [;; expression ctors
                                     ->literal ->variable ->lambda ->apply ->cons ->let ->if
                                     ->plus ->minus ->times ->divide ->lift ->run ->car ->cdr ->cons ->literal?
                                     ->cons? ->empty? ->gt ->lt
                                     ;; expression predicates
                                     literal? code? cons? code? closure? ->number? ->symbol?
                                     ;; value ctors
                                     ->constant ->tuple ->closure ->code]
             :as ast]
            [meliae.patterns :refer [match* print-pattern]]))

;;
;; Normalization-by-evaluation (NBE)-style polymorphic lift operator
;;

(defrecord State [fresh block])

(def empty-state (->State 0 []))
(def state (atom empty-state))

(defn reset-block! []
  (swap! state assoc :block []))

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
  :args ::ast/value
  :ret ::ast/expression)
(defn lift [v]
  (match* [v]
    
    [(->constant n)]
    (->literal n)

    ;; (Rep[A],Rep[B]) ==> Rep[(A,B)]
    [(->tuple (->code uu) (->code vv))]
    (reflect (->cons uu vv))

    ;;  Rep[A]=>Rep[B]  ==> Rep[A=>B]
    [(->closure env2 e2)]
    (-> #(evalms (conj (vec env2)
                       (->code (fresh!))
                       (->code (fresh!)))
                 e2)
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

(defn- evalms-unary [env ctor ee]
  (ctor (evalms env ee)))

;; TODO fold neutral elements
;; TODO profit from commutativity & co (need to take care of side-effects, they need to be done in right order!)
(defn- evalms-binary [env ctor f e1 e2]
  (match* [(evalms env e1) (evalms env e2)]

    [(->constant n1) (->constant n2)]
    (->constant (f n1 n2))

    [(->code s1) (->code s2)]
    (reflectc (ctor s1 s2))

    [(->code s1) (->constant c1)] ;; TODO this is a special case, is it even correct?
    (reflectc (ctor s1 c1))
    
    [(->code s1) y]
    (throw (IllegalArgumentException. (str "Unhandled case: " ctor " " e1 " " e2)))

    [x (->code s1)]
    (throw (IllegalArgumentException. (str "Unhandled case: " ctor " " e1 " " e2)))
    ))

(defn- evalms-predicate [env ctor f e]
  (match* [(evalms env e)]
    [(->code s1)]
    (reflectc (ctor s1))

    [(->constant c1)]
    (->constant (f c1))))

(s/fdef evalms
  :args (s/cat :env ::ast/environment
               :e   ::ast/expression)
  :ret ::ast/value)
(defn evalms [env e]
  (match* [e]

    [(->literal n)]
    (->constant n)

    [(->literal? v)]
    (cond
      (literal? v) (->literal 1)
      (code? v) (throw (IllegalStateException. "Don't know what to do???"))
      true (->literal 0))
    
    [(->cons? v)]
    (evalms-predicate env ->cons? cons? v)

    [(->empty? v)]
    (evalms-predicate env ->empty? empty? v)

    [(->number? v)]
    (evalms-predicate env ->number? number? v)

    [(->symbol? v)]
    (evalms-predicate env ->symbol? symbol? v)
    
    [(->variable n s)]
    (nth env n)

    [(->cons e1 e2)]
    (->tuple (evalms env e1) (evalms env e2))

    [(->car ee)]
    (evalms-unary env ->car ee)
    
    [(->cdr ee)]
    (evalms-unary env ->cdr ee) 
    
    [(->lambda ee)]
    (->closure env ee)

    [(->let e1 e2)]
    (let [v1 (evalms env e1)]
      (evalms (conj (vec env)
                    v1)
              e2))

    [(->lift ee)]
    (liftc (evalms env ee))

    [(->apply e1 e2)]
    (match* [(evalms env e1) (evalms env e2)]
      
      [(->closure env3 e3) v2]
      (evalms (conj (vec env3)
                    (->closure env3 e3)
                    v2)
              e3)

      [(->code s1) (->code s2)]
      (reflectc (->apply s1 s2)))

    [(->if c a b)]
    (match* [(evalms env c)]
      
      [(->constant n)]
      (if n
        (evalms env a)
        (evalms env b))

      [(->code c1)]
      (reflectc (->if c1
                      (reifyc #(evalms env a))
                      (reifyc #(evalms env b)))))

    [(->plus e1 e2)]
    (evalms-binary env ->plus + e1 e2)
    
    [(->minus e1 e2)]
    (evalms-binary env ->minus - e1 e2)
    
    [(->times e1 e2)]
    (evalms-binary env ->times * e1 e2)

    [(->divide e1 e2)]
    (evalms-binary env ->divide * e1 e2)

    [(->gt e1 e2)]
    (evalms-binary env ->gt > e1 e2)

    [(->lt e1 e2)]
    (evalms-binary env ->lt < e1 e2)
    
    [(->run b ee)]
    (match* [(evalms env b)]
      
      [(->code b1)]
      (reflectc (->run b1 (reifyc #(evalms env ee))))
      
      [_]
      (evalmsg env (reify (fn [] (evalms env ee)))))

    [(->code v1)]))

(s/fdef evalmsg
  :args (s/cat :env ::ast/environment
               :e ::ast/expression)
  :ret  ::ast/expression)
(defn evalmsg [env e]
  (reifyv #(evalms env e)))

;; TODO optimizer for expressions

