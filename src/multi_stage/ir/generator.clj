(ns multi-stage.ir.generator
  (:require [multi-stage.ir.ast :refer [->literal ->let ->do ->if ->primitive-symbol ->variable
                                        ->fn ->dot ->new ->throw ->apply ->class-reference
                                        ->literal-vector ->literal-set ->literal-map]
             :as ast]
            [multi-stage.ir.value :refer [->closure ->constant]]
            [multi-stage.post.ast :as clj]
            [meliae.patterns :refer [match*]]))

(defn generate [e index->sym]
  (comment
    (meliae.patterns/print-pattern e)
    (println))
  (let [index->sym (vec index->sym)]
    (match* [e]
      
      [(->literal value)]
      (clj/smart-literal value)

      [(->literal-vector elements)]
      (clj/smart-vector elements)

      [(->literal-set elements)]
      (clj/smart-set elements)
      
      [(->literal-map elements)]
      (clj/smart-map elements)
      
      [(->do bodies)]
      (clj/smart-do (doall (map #(generate % index->sym) bodies)))
      
      [(->let binding body original-symbol)]
      (let [sym (gensym original-symbol)]
        (clj/smart-let* [[sym (generate binding index->sym)]]
          [(generate body (conj index->sym sym))]))

      [(->if condition then else)]
      (clj/smart-if (generate condition index->sym)
                    (generate then index->sym)
                    (generate else index->sym))

      [(->primitive-symbol f)]
      (clj/smart-literal f) ;; TODO or smart-symbol?
      
      [(->variable level)]
      (let [sym (or (get index->sym level)
                    (throw (IllegalArgumentException. (str "Could not get variable " level))))]
        (clj/smart-variable sym))

      [(->fn arity ee original-function-symbol original-argument-symbols)]
      (let [f-sym (gensym original-function-symbol)
            arg-syms (map gensym original-argument-symbols)]
        (clj/smart-fn* f-sym
                       {:args (vec arg-syms)
                        :bodies [(generate ee (into (conj index->sym f-sym) arg-syms))]}))

      [(->apply ff arguments)]
      (clj/smart-invoke (generate ff index->sym)
                        (map #(generate % index->sym) arguments))

      [(->dot object method-name arguments)]
      (clj/smart-dot (generate object index->sym)
                     method-name
                     (map #(generate % index->sym) arguments))

      [(->class-reference class-name)]
      (clj/smart-class-reference class-name)
      
      [(->throw exception)]
      (clj/smart-throw (generate exception index->sym))

      [(->new class-name arguments)]
      (clj/smart-new class-name (vec (map #(generate % index->sym) arguments)))
      
      [(->closure arity env ee original-function-symbol original-argument-symbols)]
      (if (seq env)
        (throw (IllegalArgumentException. "I have no idea how to translate a closure which is not at root level"))
        (let [f-sym (gensym original-function-symbol)
              arg-syms (map gensym original-argument-symbols)]
          (clj/smart-fn* f-sym
                         {:args (vec arg-syms)
                          :bodies [(generate ee (into (conj index->sym f-sym) arg-syms))]}))))))
