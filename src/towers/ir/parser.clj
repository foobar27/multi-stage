(ns towers.ir.parser
  (:require [towers.ir.ast :refer  [->literal ->variable ->do ->let ->lambda ->apply ->dot ->new
                                    ->if ->lift ->run ->primitive-call ->quote ->throw ->class-reference]]
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

(defmulti destructured-sexp->ir (fn [sym destructured sym->index recur-target-variable]
                                  sym))

(def primitive-fns #{`get `seq `seq? `chunked-seq? `first `chunk-first `rest `chunk-rest `next `nth
                     `+ `- `* `/ `inc `dec
                     `= `< `> `int `long `str `count
                     `unchecked-inc `unchecked-add `unchecked-add-int `unchecked-byte
                     `unchecked-char `unchecked-dec `unchecked-dec-int `unchecked-divide-int
                     `unchecked-double `unchecked-float})

(defn sexp->ir [sexp sym->index recur-target-variable]
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
        (if (class? (resolve sexp ))
          (->class-reference sexp))
        (throw (IllegalArgumentException. (str "Unknown symbol: " sexp))))

    (seq? sexp)
    (let [[f & args] sexp]
      (or
       (when (symbol? f)
         (or
          ;; Clojure special form (unresolved symbol)
          (when-let [destructured (destructure-clj f args)]
            (destructured-sexp->ir f destructured sym->index recur-target-variable))
          ;; Component-wise constructors
          (let [resolved-f (resolve-symbol f)
                parsed-args (map #(sexp->ir % sym->index recur-target-variable) args)]
            (or
             (when-let [ctor (get {`lift ->lift, `run ->run} resolved-f)]
               (apply ctor parsed-args))
             ;; Primitive function call
             (when (contains? primitive-fns resolved-f)
               (->primitive-call resolved-f parsed-args))))))
       ;; Function call
       (when-let [resolved-f (sexp->ir f sym->index recur-target-variable)]
         (let [args (or (seq (map #(sexp->ir % sym->index recur-target-variable) args))
                        ;; no-arg functions get an implicit nil-argument
                        [(->literal nil)])]
           (reduce #(->apply %1 [%2])
                   resolved-f
                   args)))
       (throw (IllegalArgumentException. "Unknown symbol: " f))))

    true
    (throw (IllegalArgumentException. (str "Don't know how to parse: " sexp)))))

;; also used for implicit-do blocks
(defmethod destructured-sexp->ir 'do [_ {:keys [bodies]} sym->index recur-target-variable]
  (let [bodies (or (seq (map #(sexp->ir % sym->index recur-target-variable)
                             bodies))
                   [(->literal nil)])]
    (condp = (count bodies)
      1 (first bodies) ;; skip do-block if only one statement
      (->do bodies))))

(defmethod destructured-sexp->ir 'let* [_ {:keys [bindings bodies]} sym->index recur-target-variable]
  (if-let [[[k v] & bindings] bindings]
    (let [v (sexp->ir v sym->index recur-target-variable) ;; must be done with the old sym->index
          sym->index (push-var sym->index k)]
      (->let v
             (destructured-sexp->ir 'let*
                                    {:bindings bindings ;; remaining bindings
                                     :bodies bodies}
                                    sym->index
                                    recur-target-variable)))
    ;; base case of recursion (implicit do)
    (destructured-sexp->ir 'do
                           {:bodies bodies}
                           sym->index
                           recur-target-variable)))

(comment
  (loop [x1 v1
         x2 v2
         x3 v3]
    body)

  ((fn [x1 x2 x3]
     body)
   v1 v2 v3)
  
  (->apply (->apply (->apply f v1)
                    v2)
           v3))

(defmethod destructured-sexp->ir 'loop* [_ {:keys [bindings bodies]} sym->index recur-target-variable]
  ;; TODO this is more or less copy & paste of function application
  (let [f-name (gensym "loop")
        f (destructured-sexp->ir 'fn*
                                 {:name f-name
                                  :arities {(count bindings) {:args (vec (map first bindings))
                                                              :bodies bodies}}}
                                 sym->index
                                 f-name)]
    (first
     (reduce (fn [[f sym->index] [sym exp]]
               ;; We need to adjust sym->index continuously during the reduction
               ;; because one binding could refer to a previous binding
               ;; (like in a let*-statement)
               [(->apply f [(sexp->ir exp sym->index recur-target-variable)])
                (push-var sym->index sym)])
             [f sym->index]
             bindings))))

(defmethod destructured-sexp->ir 'recur [_ {:keys [arguments]} sym->index recur-target-variable]
  ;; TODO this is more or less copy & paste of function application
  (println "GET-VAR" sym->index recur-target-variable)
  (reduce #(->apply %1 [%2])
          recur-target-variable
          (map #(sexp->ir % sym->index recur-target-variable) ;; TODO it might be wise to reset recur-target-variable for the args?
               arguments)))

(defmethod destructured-sexp->ir 'fn* [_ {:keys [name arities]} sym->index recur-target-variable]
  (let [name (or name (gensym "unnamed"))
        [_ {:keys [args & bodies]} & _] (first arities)] ;; TODO support multiple aritiess
    (if-let [[arg & args] args]
      (let [sym->index (-> sym->index
                           (push-var name)
                           (push-var arg))
            recur-target-variable (get-var sym->index name)]
        (->lambda (if (seq args)
                    (destructured-sexp->ir 'fn*
                                           {;; Recursion should point to first lambda only
                                            ;; So we need to give "unused" names to the other lambdas
                                            :name nil 
                                            :arities {(count args) {:args args :bodies bodies}}}
                                           sym->index
                                           recur-target-variable)
                    ;; Base case of recursion (1 argument)
                    (destructured-sexp->ir 'do
                                           {:bodies bodies}
                                           sym->index
                                           recur-target-variable))))
      ;; Introduce dummy argument for 0-arg function
      (destructured-sexp->ir 'fn*
                             {:name name
                              :arities {1 {:args [(gensym "unused")] ;; TODO this arity might clash with 1-arity
                                           :bodies bodies}}}
                             sym->index
                             recur-target-variable))))

(defmethod destructured-sexp->ir 'if [_ {:keys [condition then else]} sym->index recur-target-variable]
  (->if (sexp->ir condition sym->index recur-target-variable)
        (sexp->ir then      sym->index recur-target-variable)
        (sexp->ir else      sym->index recur-target-variable)))

(comment
  (macroexpand `(.writeByte output (int ~'data))))

(defmethod destructured-sexp->ir '. [_ [object method-name & args] sym->index recur-target-variable]
  (->dot (sexp->ir object sym->index recur-target-variable)
         method-name
         (doall (map #(sexp->ir % sym->index recur-target-variable) args))))

(defmethod destructured-sexp->ir 'new [_ {:keys [class-name arguments]} sym->index recur-target-variable]
  (->new class-name
         (doall (map #(sexp->ir % sym->index recur-target-variable) arguments))))

(defmethod destructured-sexp->ir 'throw [_ {:keys [exception]} sym->index recur-target-variable]
  (->throw (sexp->ir exception sym->index recur-target-variable)))

(defn clj->ir [sexp]
  (sexp->ir (macroexpand-all sexp) {} nil))

(defmacro parse [sexp]
  (clj->ir sexp))
