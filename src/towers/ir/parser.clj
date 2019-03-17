(ns towers.ir.parser
  (:require [towers.ir.ast :refer  [->literal ->variable ->do ->let ->lambda ->apply ->if ->lift ->run ->primitive-call ->quote]]
            [clojure.walk :refer [macroexpand-all]]
            [towers.utils :refer [resolve-symbol]]
            [towers.clojure.parser :refer [destructure-clj]]
            [towers.ir.core :refer [lift run]]))

(defn- get-var [sym->index s]
  (if-let [i (get sym->index s)]
    (->variable i (name s))))

(defn- push-var [sym->index s]
  (assoc sym->index
         s (count sym->index)))

(declare sexp->ir)

(defmulti destructured-sexp->ir (fn [sym destructured sym->index]
                                  sym))

(def primitive-fns #{`get `seq `first `rest `=})

(defn sexp->ir [sexp sym->index]
  (cond
    (or (string? sexp)
        (keyword? sexp)
        (char? sexp)
        (number? sexp)
        (nil? sexp)
        (contains? #{true false} sexp))
    (->literal sexp)
    
    (symbol? sexp)
    (or (get-var sym->index sexp)
        (throw (IllegalArgumentException. (str "Unknown symbol: " sexp))))

    (seq? sexp)
    (let [[f & args] sexp]
      (or
       (when (symbol? f)
         (or
          ;; Clojure special form (unresolved symbol)
          (when-let [destructured (destructure-clj f args)]
            (destructured-sexp->ir f destructured sym->index))
          ;; Component-wise constructors
          (let [resolved-f (resolve-symbol f)
                parsed-args (map #(sexp->ir % sym->index) args)]
            (or
             (when-let [ctor (get {`lift ->lift, `run ->run} resolved-f)]
               (apply ctor parsed-args))
             ;; Primitive function call
             (when (contains? primitive-fns resolved-f)
               (->primitive-call resolved-f parsed-args))))))
       ;; Function call
       (when-let [resolved-f (sexp->ir f sym->index)]
         ;; TODO this should be reduce ->apply
         (let [args (or (seq (map #(sexp->ir % sym->index) args))
                        ;; no-arg functions get an implicit nil-argument
                        [(->literal nil)])]
           (reduce ->apply resolved-f args)))
       (throw (IllegalArgumentException. "Unknown symbol: " f))))

    true
    (throw (IllegalArgumentException. (str "Don't know how to parse: " sexp)))))

;; also used for implicit-do blocks
(defmethod destructured-sexp->ir 'do [_ {:keys [bodies]} sym->index]
  (let [bodies (or (seq (map #(sexp->ir % sym->index)
                             bodies))
                   [(->literal nil)])]
    (condp = (count bodies)
      1 (first bodies) ;; skip do-block if only one statement
      (->do bodies))))

(defmethod destructured-sexp->ir 'let* [_ {:keys [bindings bodies]} sym->index]
  (if-let [[[k v] & bindings] bindings]
    (let [v (sexp->ir v sym->index) ;; must be done with the old sym->index
          sym->index (push-var sym->index v)]
      (->let v
             (destructured-sexp->ir 'let* {:bindings bindings ;; remaining bindings
                                           :bodies bodies}
                                    sym->index)))
    ;; base case of recursion (implicit do)
    (destructured-sexp->ir 'do {:bodies bodies} sym->index)))

(defmethod destructured-sexp->ir 'fn* [_ {:keys [name arities]} sym->index]
  (let [name (or name (gensym "unnamed"))
        [_ {:keys [args & bodies]} & _] (first arities)] ;; TODO support multiple aritiess
    (if-let [[arg & args] args]
      (let [sym->index (-> sym->index
                           (push-var name)
                           (push-var arg))]
        (->lambda (if (seq args)
                    (destructured-sexp->ir 'fn*
                                           {;; Recursion should point to first lambda only
                                            ;; So we need to give "unused" names to the other lambdas
                                            :name nil 
                                            :arities {(count args) {:args args :bodies bodies}}}
                                           sym->index)
                    ;; Base case of recursion (1 argument)
                    (destructured-sexp->ir 'do {:bodies bodies} sym->index))))
      ;; Introduce dummy argument for 0-arg function
      (destructured-sexp->ir 'fn* {:name name
                                   :arities {1 {:args [(gensym "unused")] ;; TODO this arity might clash with 1-arity
                                                :bodies bodies}}}
                             sym->index))))

(defmethod destructured-sexp->ir 'if [_ {:keys [condition then else]} sym->index]
  (->if (sexp->ir condition sym->index)
        (sexp->ir then      sym->index)
        (sexp->ir else      sym->index)))

(defn clj->ir [sexp]
  (sexp->ir (macroexpand-all sexp) {}))

(defmacro parse [sexp]
  (clj->ir sexp))




