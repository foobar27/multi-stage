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

(defmulti destructured-sexp->ir (fn [sym destructured sym->index recur-target-index]
                                  sym))

(def primitive-fns #{`get `seq `seq? `chunked-seq? `first `chunk-first `rest `chunk-rest `next
                     `= `< `> `int `long `str `count
                     `unchecked-inc `unchecked-add `unchecked-add-int `unchecked-byte
                     `unchecked-char `unchecked-dec `unchecked-dec-int `unchecked-divide-int
                     `unchecked-double `unchecked-float})

(defn sexp->ir [sexp sym->index recur-target-index]
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
            (destructured-sexp->ir f destructured sym->index recur-target-index))
          ;; Component-wise constructors
          (let [resolved-f (resolve-symbol f)
                parsed-args (map #(sexp->ir % sym->index recur-target-index) args)]
            (or
             (when-let [ctor (get {`lift ->lift, `run ->run} resolved-f)]
               (apply ctor parsed-args))
             ;; Primitive function call
             (when (contains? primitive-fns resolved-f)
               (->primitive-call resolved-f parsed-args))))))
       ;; Function call
       (when-let [resolved-f (sexp->ir f sym->index recur-target-index)]
         (let [args (or (seq (map #(sexp->ir % sym->index recur-target-index) args))
                        ;; no-arg functions get an implicit nil-argument
                        [(->literal nil)])]
           (reduce #(->apply %1 [%2]) resolved-f args)))
       (throw (IllegalArgumentException. "Unknown symbol: " f))))

    true
    (throw (IllegalArgumentException. (str "Don't know how to parse: " sexp)))))

;; also used for implicit-do blocks
(defmethod destructured-sexp->ir 'do [_ {:keys [bodies]} sym->index recur-target-index]
  (let [bodies (or (seq (map #(sexp->ir % sym->index recur-target-index)
                             bodies))
                   [(->literal nil)])]
    (condp = (count bodies)
      1 (first bodies) ;; skip do-block if only one statement
      (->do bodies))))

(defmethod destructured-sexp->ir 'let* [_ {:keys [bindings bodies]} sym->index recur-target-index]
  (if-let [[[k v] & bindings] bindings]
    (let [v (sexp->ir v sym->index recur-target-index) ;; must be done with the old sym->index
          sym->index (push-var sym->index k)]
      (->let v
             (destructured-sexp->ir 'let*
                                    {:bindings bindings ;; remaining bindings
                                     :bodies bodies}
                                    sym->index
                                    recur-target-index)))
    ;; base case of recursion (implicit do)
    (destructured-sexp->ir 'do
                           {:bodies bodies}
                           sym->index
                           recur-target-index)))

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

(defmethod destructured-sexp->ir 'loop* [_ {:keys [bindings bodies]} sym->index recur-target-index]
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
               (let [sym->index (push-var sym->index sym)])
               [(->apply f (sexp->ir exp sym->index recur-target-index)) sym->index])
             [f sym->index]
             bindings))))

(defmethod destructured-sexp->ir 'recur [_ {:keys [arguments]} sym->index recur-target-index]
  ;; TODO this is more or less copy & paste of function application
  (reduce ->apply
          (->variable recur-target-index (get-var sym->index recur-target-index))
          (map #(sexp->ir % sym->index recur-target-index)
               arguments)))

(defmethod destructured-sexp->ir 'fn* [_ {:keys [name arities]} sym->index recur-target-index]
  (let [name (or name (gensym "unnamed"))
        [_ {:keys [args & bodies]} & _] (first arities)] ;; TODO support multiple aritiess
    (if-let [[arg & args] args]
      (let [sym->index (-> sym->index
                           (push-var name)
                           (push-var arg))
            recur-target-index (get-var sym->index name)]
        (->lambda (if (seq args)
                    (destructured-sexp->ir 'fn*
                                           {;; Recursion should point to first lambda only
                                            ;; So we need to give "unused" names to the other lambdas
                                            :name nil 
                                            :arities {(count args) {:args args :bodies bodies}}}
                                           sym->index
                                           recur-target-index)
                    ;; Base case of recursion (1 argument)
                    (destructured-sexp->ir 'do
                                           {:bodies bodies}
                                           sym->index
                                           recur-target-index))))
      ;; Introduce dummy argument for 0-arg function
      (destructured-sexp->ir 'fn*
                             {:name name
                              :arities {1 {:args [(gensym "unused")] ;; TODO this arity might clash with 1-arity
                                           :bodies bodies}}}
                             sym->index
                             recur-target-index))))

(defmethod destructured-sexp->ir 'if [_ {:keys [condition then else]} sym->index recur-target-index]
  (->if (sexp->ir condition sym->index recur-target-index)
        (sexp->ir then      sym->index recur-target-index)
        (sexp->ir else      sym->index recur-target-index)))

(comment
  (macroexpand `(.writeByte output (int ~'data))))

(defmethod destructured-sexp->ir '. [_ [object method-name & args] sym->index recur-target-index]
  (->dot (sexp->ir object sym->index recur-target-index)
         method-name
         (doall (map #(sexp->ir % sym->index recur-target-index) args))))

(defmethod destructured-sexp->ir 'new [_ {:keys [class-name arguments]} sym->index recur-target-index]
  (->new class-name
         (doall (map #(sexp->ir % sym->index recur-target-index) arguments))))

(defmethod destructured-sexp->ir 'throw [_ {:keys [exception]} sym->index recur-target-index]
  (->throw (sexp->ir exception sym->index recur-target-index)))

(defn clj->ir [sexp]
  (sexp->ir (macroexpand-all sexp) {} nil))

(defmacro parse [sexp]
  (clj->ir sexp))
