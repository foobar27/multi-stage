(ns multi-stage.ir.parser
  (:require [multi-stage.ir.ast :refer [->literal ->variable ->do ->let ->lambda ->apply ->dot ->new
                                        ->if ->lift ->run ->primitive-call ->quote ->throw ->class-reference]
             :as ast]
            [clojure.walk :refer [macroexpand-all]]
            [meliae.patterns :refer [match*]]
            [multi-stage.utils :refer [resolve-symbol]]
            [multi-stage.clojure.parser :refer [destructure-clj]]
            [multi-stage.ir.core :refer [lift lift-loop run]]))

(defn- get-var [sym->index s]
  (if-let [i (get sym->index s)]
    (->variable i)))

(defn- push-var [sym->index s]
  (assoc sym->index
         s (if-let [xs (vals sym->index)]
             (inc (apply max xs))
             0)))

(declare sexp->ir)

(defmulti destructured-sexp->ir (fn [sym destructured sym->index recur-target-variable]
                                  sym))

(def primitive-fns #{`get `seq `seq? `chunked-seq? `first `chunk-first `rest `chunk-rest `next `nth
                     `assoc! `conj! `transient `persistent!
                     `+ `- `* `/ `inc `dec `pos?
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
             (when (= `lift-loop resolved-f)
               (match* [(vec parsed-args)]
                 [[(->apply the-lambda lambda-arguments)]]
                 (->apply (->lift the-lambda) lambda-arguments)))
             ;; Primitive function call
             (when (contains? primitive-fns resolved-f)
               (->primitive-call resolved-f parsed-args))))))
       ;; Function call
       (when-let [resolved-f (sexp->ir f sym->index recur-target-variable)]
         (let [args (seq (map #(sexp->ir % sym->index recur-target-variable) args))]
           (->apply resolved-f args)))
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
  (if (seq bindings)
    (let [[[k v] & bindings] bindings]
      (let [v (sexp->ir v sym->index recur-target-variable) ;; must be done with the old sym->index
            sym->index (push-var sym->index k)]
        (->let v
               (destructured-sexp->ir 'let*
                                      {:bindings bindings ;; remaining bindings
                                       :bodies bodies}
                                      sym->index
                                      recur-target-variable)
               k)))
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
   v1 v2 v3))

(defmethod destructured-sexp->ir 'loop* [_ {:keys [bindings bodies]} sym->index recur-target-variable]
  ;; TODO this is more or less copy & paste of function application
  (let [f-name (gensym "loop")
        f (destructured-sexp->ir 'fn*
                                 {:name f-name
                                  :arities {(count bindings) {:args (vec (map first bindings))
                                                              :bodies bodies}}}
                                 sym->index
                                 f-name)]
    (loop [sym->index sym->index
           bindings bindings
           arguments []]
      (if (seq bindings)
        (let [[[sym exp] & bindings] bindings]
          ;; We need to adjust sym->index continuously during the iteration
          ;; because one binding could refer to a previous binding
          ;; (like in a let*-statement)
          (let [argument (sexp->ir exp sym->index recur-target-variable)
                sym->index (push-var sym->index sym)]
            (recur sym->index bindings (conj arguments argument))))
        (->apply f arguments)))))

(defmethod destructured-sexp->ir 'recur [_ {:keys [arguments]} sym->index recur-target-variable]
  (->apply recur-target-variable
           (map #(sexp->ir % sym->index recur-target-variable)
                arguments)))

(defmethod destructured-sexp->ir 'fn* [_ {:keys [name arities]} sym->index recur-target-variable]
  (let [name (or name (gensym "unnamed"))
        [_ {:keys [args & bodies]} & _] (first arities)] ;; TODO support multiple arities
    (let [sym->index (reduce push-var
                             (push-var sym->index name)
                             args)
          recur-target-variable (get-var sym->index name)]
      (->lambda (count args)
                (destructured-sexp->ir 'do
                                       {:bodies bodies}
                                       sym->index
                                       recur-target-variable)
                name
                args))))

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

(defmethod destructured-sexp->ir 'quote [_ {:keys [value]} sym->index recur-target-variable]
  (->quote value))

(defn clj->ir [sexp]
  (sexp->ir (macroexpand-all sexp) {} nil))

(defmacro parse [sexp]
  (clj->ir sexp))
