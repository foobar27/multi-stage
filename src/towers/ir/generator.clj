(ns towers.ir.generator
  (:require [towers.ir.ast :refer [->literal ->let ->if ->primitive-call ->variable ->closure] :as ast]
            [meliae.patterns :refer [match*]]))

(defn generate [e index->sym]
  (let [index->sym (vec index->sym)]
    (print "GENERATE WITH MAP" index->sym "EXPRESSION ")
    (meliae.patterns/print-pattern e)
    (println)
    (match* [e]
      [(->literal n)]
      (cond
        (number? n) n
        (string? n) n
        (keyword? n) n
        (#{true false} n) n
        true  `(quote ~n))

      [(->let e1 e2)]
      (let [sym (gensym)]
        `(let* [~sym ~(generate e1 index->sym)]
           ~(generate e2 (conj index->sym sym))))

      [(->if condition then else)]
      `(if ~(generate condition index->sym)
         ~(generate then index->sym)
         ~(generate else index->sym))

      [(->primitive-call f args)]
      `(~f ~@(into [] (map #(generate % index->sym) args)))

      [(->variable level original-name)]
      (or (get index->sym level)
          (throw (IllegalArgumentException. (str "Could not get variable " level " with original name " original-name))))

      [(->closure env ee)]
      (if (seq env)
        ;; TODO how can this happen? What shall we do?
        ;; TODO Maybe in a let block, if a lambda is passed by argument, or if a lambda is returned?
        (throw (IllegalArgumentException. "I have no idea how to translate a closure which is not at root level")) 
        (let [f-sym (gensym)
              sym (gensym)]
          `(fn* ~f-sym [~sym]
                ~(generate ee (conj index->sym f-sym sym))))))))

;; TODO ->apply
;; TODO ->lambda


