(ns towers.clojure.generator
  (:require [towers.clojure.ast :refer [->do ->let* ->fn* ->literal ->variable ->if ->invoke] :as ast]
            [meliae.patterns :refer [match*]]))

(defn generate [e]
  (match* [e]
    
    [(->literal value)]
    (if ((some-fn number? string? keyword? boolean?) value)
      value
      `(quote ~value))

    [(->let* bindings bodies)]
    `(let* [~@(doall (mapcat (fn [[sym expr]]
                               [sym (generate expr)])
                             bindings))]
       ~@(doall (map generate bodies)))

    [(->do bodies)]
    `(do ~@(doall (map generate bodies)))

    [(->if condition then (->literal nil))]
    `(if ~(generate condition)
       ~(generate then))

    [(->if condition then else)]
    `(if ~(generate condition)
       ~(generate then)
       ~(generate else))

    [(->variable symbol)]
    symbol

    [(->invoke f args)]
    `(~(generate f) ~@(doall (map generate args)))

    [(->fn* f-name signatures)]
    (if (= 1 (count signatures))
      ;; Use simplified form if only one signature.
      `(fn* ~f-name
            ~(let [{:keys [args bodies]} (first signatures)]
               `([~@args]
                 ~@(doall (map generate bodies)))))
      ;; Use variadic form if more than one signature
      `(fn* ~f-name
            ~@(doall (for [{:keys [args bodies]} signatures]
                       `([~@args]
                         ~@(doall (map generate bodies)))))))))
