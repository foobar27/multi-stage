(ns towers.base.parser
  (:require [towers.base.ast :refer [->literal ->lambda ->let ->variable ->cons ->plus ->minus ->times ->divide ->apply ->if
                                     ->nil? ->equals? ->TRUE ->FALSE ->NIL ->car ->cdr ->empty? ->number? ->symbol?
                                     ->lift ->gt ->run]]
            [clojure.walk :refer [macroexpand-all]]
            [towers.utils :refer [resolve-symbol]]
            [towers.clojure.parser :refer [destructure-clj]]))

(defn- get-var [sym->index s]
  (if-let [i (get sym->index s)]
    (->variable i (name s))))

(defn- push-var [sym->index s]
  (assoc sym->index
         s (count sym->index)))

(declare sexp->expression)

(defmulti sexp->expression-resolved-dispatch (fn [f args sym->index] f) :default ::undefined) ;; TODO line number

(defmulti sexp->expression-unresolved-dispatch (fn [f args sym->index] f) :default ::undefined)

(defmulti native-fn->expression (fn [f sym->index] f) :default ::undefined)

(defn sexp->expression [expr sym->index]
  (do
    (cond
      (number? expr)
      (->literal expr)
      
      (symbol? expr)
      (or (get-var sym->index expr)
          (native-fn->expression expr sym->index)
          (throw (IllegalArgumentException. (str "Unknown variable: " expr " (keys: " (keys sym->index) ")"))))
      
      (seq? expr)
      (let [[f & args] expr
            apply-lambda (fn [f]
                           (let [args (map #(sexp->expression % sym->index) args)
                                 args (if (seq args) args [(->literal 0)])]
                             (reduce (fn [out arg] (->apply out arg)) f args)))]
        (if (symbol? f)
          (if-let [resolved-f (get-var sym->index f)]
            ;; resolved via sym->index: apply function
            (apply-lambda resolved-f)
            ;; resolved via clojure namespaces
            (if-let [resolved-f (resolve-symbol f)]
              (sexp->expression-resolved-dispatch resolved-f args sym->index)
              (sexp->expression-unresolved-dispatch f args sym->index)))
          ;; apply expression
          (let [f (sexp->expression f sym->index)]
            (apply-lambda f))))

      true (throw (IllegalArgumentException. (str "Don't know how to translate: " expr))))))

(defn- arg-recur [ctor args sym->index]
  (apply ctor (map #(sexp->expression % sym->index) args)))

(defn- arg-recur-2-associative [ctor neutral args sym->index]
  (condp = (count args)
    0 neutral
    1 (sexp->expression (first args) sym->index)
    2 (arg-recur ctor args sym->index)
    (let [[a & args] args]
      (ctor (sexp->expression a sym->index)
            (arg-recur-2-associative ctor neutral args sym->index)))))

(defmethod sexp->expression-resolved-dispatch `= [f args sym->index]
  (condp = (count args)
    0 (throw (IllegalAccessError. "Need at least one argument for ="))
    1 (->TRUE)
    (arg-recur-2-associative ->equals? nil args sym->index)
    ))

(def ^:private elementwise-ctors
  {`cons   ->cons
   `first  ->car
   `rest   ->cdr
   `nil?   ->nil?
   `empty? ->empty?
   `number?  ->number?
   `symbol? ->symbol?
   `>      ->gt
   `towers.base.interpreter/lift   ->lift ;; TODO this should be in a different namespace (e.g. towers.base.language)
   `true   ->TRUE
   `false  ->FALSE
   `nil    ->NIL})

(def ^:private n-ary-associative-ctors
  {`+ [->plus  (->literal 0)]
   `- [->minus (->literal 0)]
   `* [->times (->literal 1)]})

(defmethod sexp->expression-resolved-dispatch ::undefined [f args sym->index]
  (if-let [ctor (get elementwise-ctors f)]
    (arg-recur ctor args sym->index)
    (if-let [[ctor neutral] (get n-ary-associative-ctors f)]
      (arg-recur-2-associative ctor neutral args sym->index)
      (throw (IllegalArgumentException. (str "Unhandled resolved symbol: " f))))))

(defmethod native-fn->expression ::undefined [f sym->index]
  (letfn [(build-lambda [ctor n]
            (let [var-names (map (fn [i] (str "native-arg" i))
                                 (range n))
                  sym->index (reduce push-var sym->index var-names)]
              (nth (iterate ->lambda (apply ctor (map (partial get-var sym->index)
                                                      var-names)))
                   n)))]
    (condp = (resolve-symbol f) ;; TODO implement via multi methods
      ;; TODO this could be done better via variadic functions
      ;; TODO extend to other native functions
      `number? (build-lambda ->number? 1)
      `symbol? (build-lambda ->symbol? 1)
      `first (build-lambda ->car 1)
      `rest (build-lambda ->cdr 1)
      `cons (build-lambda ->cons 1)
      `+ (build-lambda ->plus 2)
      `- (build-lambda ->minus 2)
      `* (build-lambda ->times 2)
      `/ (build-lambda ->divide 2)
      `= (build-lambda ->equals? 2))))

(defmethod sexp->expression-unresolved-dispatch ::undefined [f _ _]  
  (throw (IllegalArgumentException. (str "Unhandled unresolved symbol: " f)))) ;; TODO line number?

;; TODO support multiple arities syntax
;; TODO support recur
(defn- destructured-fn*->expression [{:keys [name arities]} sym->index]
  (let [name (or name
                 (gensym "unnamed"))
        [_ {:keys [args body]} & _] (first arities)] ;; TODO support multiple arities
    (condp = (count args)
      0 (destructured-fn*->expression {:name name
                                       :arities {1 {:args [(gensym "unused")]
                                                    :body body}}}
                                      sym->index)
      1 (let [arg (first args)
              sym->index (-> sym->index (push-var name) (push-var arg))]
          ;; TODO support multiple statements in body
          (->lambda (sexp->expression (first body) sym->index)))
      ;; else
      (let [[arg & args] args
            sym->index (-> sym->index (push-var name) (push-var arg))]
        (->lambda (destructured-fn*->expression {;; Recursion should point to first lambda only
                                                 ;; So we need to give "unused" names to the other lambdas
                                                 :name nil 
                                                 :arities {(count args) {:args args
                                                                         :body body}}}
                                                sym->index))))))

(defmethod sexp->expression-unresolved-dispatch 'fn* [_ args sym->index]
  (destructured-fn*->expression (destructure-clj 'fn* args) sym->index))

(defmethod sexp->expression-unresolved-dispatch 'let* [_ args sym->index]
  (let [[bindings body] args
        bindings (partition 2 bindings)]
    (if (empty? bindings)
      (sexp->expression body sym->index)
      (let [[[v x] & bindings] bindings
            x (sexp->expression x sym->index) ;; must be done with the old sym->index
            sym->index (push-var sym->index v)]
        (->let x
               (sexp->expression (if (empty? bindings)
                                   ;; there has only been 1 binding in total
                                   body
                                   ;; more bindings -> reduce to 1 binding
                                   `(let* [~@(apply concat bindings)] ~body))
                                 sym->index))))))

(defmethod sexp->expression-unresolved-dispatch 'if [_ args sym->index]
  (let [n (count args)]
    (if (= 3 n)
      (arg-recur ->if args sym->index)
      (throw (IllegalArgumentException. (str "Can only translate if-statements with two code paths, got " (dec n) ": " args))))))

(defmethod sexp->expression-unresolved-dispatch 'quote [_ args sym->index]
  (reduce #(->cons %2 %1) nil (reverse args)))

;; TODO shouldn't this be 'resolved'?
(defmethod sexp->expression-unresolved-dispatch 'run [_ args sym->index]
  (apply ->run (map #(sexp->expression % sym->index) args)))

(defn clj->expression [code]
  (let [code (macroexpand-all code)]
    ;;(println "compiling" code)
    (sexp->expression code {})))

(defmacro parse [code]
  (clj->expression code))
