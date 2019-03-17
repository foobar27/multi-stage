(ns towers.ir.generator
  (:require [towers.ir.ast :refer [->literal ->let ->if ->primitive-call ->variable ->closure] :as ast]
            [towers.clojure.ast :as clj]
            [meliae.patterns :refer [match*]]))

(defn generate [e index->sym]
  (let [index->sym (vec index->sym)]
    (match* [e]
      
      [(->literal value)]
      (clj/smart-literal value)

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

      [(->closure env ee)]
      (if (seq env)
        (throw (IllegalArgumentException. "I have no idea how to translate a closure which is not at root level"))
        (let [f-sym (gensym)
              arg-sym (gensym)]
          (clj/smart-fn* f-sym
                         {:args [arg-sym] ;; TODO support multiple arguments
                          :bodies [(generate ee (conj index->sym f-sym arg-sym))]}))))))

;; TODO ->apply
;; TODO ->lambda
