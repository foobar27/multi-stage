(ns towers.ir.generator
  (:require [towers.ir.ast :refer [->literal ->let ->do ->if ->primitive-call ->variable ->closure
                                   ->lambda ->dot ->new ->throw ->apply ->class-reference]
             :as ast]
            [towers.clojure.ast :as clj]
            [meliae.patterns :refer [match*]]))

(defn generate [e index->sym]
  (meliae.patterns/print-pattern e)
  (println)
  (let [index->sym (vec index->sym)]
    (match* [e]
      
      [(->literal value)]
      (clj/smart-literal value)

      [(->do bodies)]
      (clj/smart-do (doall (map #(generate % index->sym) bodies)))
      
      [(->let binding body original-name)]
      (let [sym (gensym original-name)]
        (clj/smart-let* [[sym (generate binding index->sym)]]
          [(generate body (conj index->sym sym))]))

      [(->if condition then else)]
      (clj/smart-if (generate condition index->sym)
                    (generate then index->sym)
                    (generate else index->sym))

      [(->primitive-call f args)]
      (clj/smart-invoke (clj/smart-literal f) ;; TODO or smart-symbol?
                        (doall (map #(generate % index->sym) args)))

      [(->variable level)]
      (let [sym (or (get index->sym level)
                    (throw (IllegalArgumentException. (str "Could not get variable " level))))]
        (clj/smart-variable sym))

      [(->lambda arity ee original-function-name original-argument-names)]
      (let [f-sym (gensym original-function-name)
            arg-syms (map gensym original-argument-names)]
        (clj/smart-fn* f-sym
                       {:args (vec arg-syms)
                        :bodies [(generate ee (into (conj index->sym f-sym) arg-syms))]}))

      [(->apply ff arguments)]
      (clj/smart-invoke (generate ff index->sym)
                        (map #(generate % index->sym) arguments))

      [(->dot object method-name arguments)]
      (clj/->dot (generate object index->sym)
                 method-name
                 (vec (map #(generate % index->sym) arguments)))

      [(->class-reference class-name)]
      (clj/->class-reference class-name)
      
      [(->throw exception)]
      (clj/->throw (generate exception index->sym))

      [(->new class-name arguments)]
      (clj/->new class-name (vec (map #(generate % index->sym) arguments)))
      
      [(->closure arity env ee original-function-name original-argument-names)]
      (if (seq env)
        (throw (IllegalArgumentException. "I have no idea how to translate a closure which is not at root level"))
        (let [f-sym (gensym original-function-name)
              arg-syms (map gensym original-argument-names)]
          (clj/smart-fn* f-sym
                         {:args (vec arg-syms)
                          :bodies [(generate ee (into (conj index->sym f-sym) arg-syms))]}))))))
