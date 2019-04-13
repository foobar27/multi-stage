(ns towers.ir.generator
  (:require [towers.ir.ast :refer [->literal ->let ->do ->if ->primitive-call ->variable ->closure
                                   ->lambda ->dot ->new ->throw ->apply ->class-reference]
             :as ast]
            [towers.clojure.ast :as clj]
            [meliae.patterns :refer [match*]]))

(defn generate [e index->sym]
  (println "GENERATE")
  (meliae.patterns/print-pattern e)
  (println)
  (let [index->sym (vec index->sym)]
    (match* [e]
      
      [(->literal value)]
      (clj/smart-literal value)

      [(->do bodies)]
      (clj/smart-do (doall (map #(generate % index->sym) bodies)))
      
      [(->let binding body)]
      (let [sym (gensym)]
        (clj/smart-let* [[sym (generate binding index->sym)]]
          [(generate body (conj index->sym sym))]))

      [(->if condition then else)]
      (clj/smart-if (generate condition index->sym)
                    (generate then index->sym)
                    (generate else index->sym))

      [(->primitive-call f args)]
      (clj/smart-invoke (clj/smart-literal f) ;; TODO or smart-symbol?
                        (doall (map #(generate % index->sym) args)))

      [(->variable level original-name)]
      (let [sym (or (get index->sym level)
                    (throw (IllegalArgumentException. (str "Could not get variable " level
                                                           " with original name " original-name))))]
        (clj/smart-variable sym))

      [(->lambda ee)]
      (let [f-sym (gensym)
            arg-sym (gensym)]
        (clj/smart-fn* f-sym
                       {:args [arg-sym]
                        :bodies [(generate ee (conj index->sym f-sym arg-sym))]}))

      [(->apply ff [ee])]
      (clj/->call (generate ff index->sym)
                  [(generate ee index->sym)])

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
      
      [(->closure env ee)]
      (if (seq env)
        (throw (IllegalArgumentException. "I have no idea how to translate a closure which is not at root level"))
        (let [f-sym (gensym)
              arg-sym (gensym)]
          (clj/smart-fn* f-sym
                         {:args [arg-sym] ;; TODO support multiple arguments
                          :bodies [(generate ee (conj index->sym f-sym arg-sym))]}))))))
