(ns multi-stage.post.generator
  (:require [multi-stage.post.ast :refer [->do ->let* ->fn* ->literal ->variable
                                          ->if ->invoke ->dot ->throw ->new ->class-reference
                                          ->literal-vector ->literal-set ->literal-map
                                          ->loop]
             :as ast]
            [meliae.patterns :refer [match*]]))

(defn generate
  ([e]
   (generate e nil))
  ([e recur-target]
   (match* [e]
     
     [(->literal value used-symbols)]
     (if ((some-fn number? string? keyword? boolean? symbol? char? nil?) value)
       value
       `(quote ~value))

     [(->literal-vector elements used-symbols)]
     (vec (map #(generate % recur-target) elements))

     [(->literal-set elements used-symbols)]
     (into #{} (map #(generate % recur-target) elements))
     
     [(->literal-map elements used-symbols)]
     (into {} (map (fn [[k v]]
                     [(generate k recur-target)
                      (generate v recur-target)])
                   elements))

     [(->let* bindings bodies used-symbols)]
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

     [(->do bodies used-symbols)]
     `(do ~@(doall (map #(generate % recur-target) bodies)))
     
     [(->if condition then (->literal nil used-symbols-literal) used-symbols-if)]
     `(if ~(generate condition recur-target)
        ~(generate then recur-target))

     [(->if condition then else used-symbols)]
     `(if ~(generate condition recur-target)
        ~(generate then recur-target)
        ~(generate else recur-target))

     [(->variable symbol used-symbols)]
     symbol

     [(->new class-name arguments used-symbols)]
     `(new ~class-name ~@(doall (map #(generate % recur-target) arguments)))

     [(->class-reference class-name used-symbols)]
     class-name
     
     [(->dot object method-name arguments used-symbols)]
     `(. ~(generate object recur-target) ~method-name ~@(doall (map #(generate % recur-target) arguments)))

     [(->throw ee used-symbols)]
     `(throw ~(generate ee recur-target))

     [(->loop loop-name bindings bodies used-symbols)]
     ;; TODO recur-target should be reset here to loop-name?
     `(loop [~@(mapcat (fn [[k v]]
                         [k (generate v recur-target)])
                       bindings)]
        ~@(map #(generate % loop-name) bodies)) 
     
     [(->invoke f args tail-call? used-symbols)]
     (let [f (generate f recur-target)]
       (if (and tail-call?
                (= f recur-target))
         ;; TODO the string-to-symbol conversion is a workaround to an alegded clojure bug
         `(~(symbol "recur") ~@(doall (map #(generate % recur-target) args)))
         `(~f ~@(doall (map #(generate % recur-target) args)))))

     [(->fn* f-name signatures used-symbols)]
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
