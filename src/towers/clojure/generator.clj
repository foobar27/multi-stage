(ns towers.clojure.generator
  (:require [towers.clojure.ast :refer [->do ->let* ->fn* ->literal ->variable
                                        ->if ->invoke ->dot ->throw ->new ->class-reference]
             :as ast]
            [meliae.patterns :refer [match*]]))

(defn generate
  ([e]
   (generate e nil))
  ([e recur-target]
   (match* [e]
     
     [(->literal value)]
     (if ((some-fn number? string? keyword? boolean? symbol? nil?) value)
       value
       `(quote ~value))

     [(->let* bindings bodies)]
     ;; recur-target might be shadowed by any binding.
     ;; Shadowing should never happen for code generated from "ir",
     ;; but we want to be sure to reset the recur target in such cases.
     (let [binding-recur-targets (reductions (fn [recur-target b]
                                               (if-not b recur-target))
                                             recur-target
                                             (for [[sym _] bindings]
                                               (= sym recur-target)))
           recur-target (last binding-recur-targets)]
       `(let* [~@(doall (mapcat (fn [[sym expr] recur-target]
                                  [sym (generate expr recur-target)])
                                bindings
                                binding-recur-targets))]
          ~@(doall (map #(generate % recur-target) bodies))))

     [(->do bodies)]
     `(do ~@(doall (map #(generate % recur-target) bodies)))
     
     [(->if condition then (->literal nil))]
     `(if ~(generate condition recur-target)
        ~(generate then recur-target))

     [(->if condition then else)]
     `(if ~(generate condition recur-target)
        ~(generate then recur-target)
        ~(generate else recur-target))

     [(->variable symbol)]
     symbol

     [(->new class-name arguments)]
     `(new ~class-name ~@(doall (map #(generate % recur-target) arguments)))

     [(->class-reference class-name)]
     class-name
     
     [(->dot object method-name arguments)]
     `(. ~(generate object recur-target) ~method-name ~@(doall (map #(generate % recur-target) arguments)))

     [(->throw ee)]
     `(throw ~(generate ee recur-target))
     
     [(->invoke f args tail-call?)]
     (if (and tail-call?
              (= f recur-target))
       ;; TODO the string-to-symbol conversion is a workaround to an alegded clojure bug
       `(~(symbol "recur") ~@(doall (map #(generate % recur-target) args)))
       `(~(generate f recur-target) ~@(doall (map #(generate % recur-target) args))))

     [(->fn* f-name signatures)]
     (if (= 1 (count signatures))
       ;; Use simplified form if only one signature.
       `(fn* ~f-name
             ~@(let [{:keys [args bodies]} (first signatures)]
                 `([~@args]
                   ~@(doall (map #(generate % f-name) bodies)))))
       ;; Use variadic form if more than one signature
       `(fn* ~f-name
             ~@(doall (for [{:keys [args bodies]} signatures]
                        `([~@args]
                          ~@(doall (map #(generate % f-name) bodies))))))))))
