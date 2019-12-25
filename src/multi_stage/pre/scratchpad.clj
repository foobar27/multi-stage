(ns multi-stage.pre.scratchpad)

(require '[clojure.core.match :refer [match]])

(comment
  
  (let [n 10]
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _ 1] "Fizz"
      [_ 0] "Buzz"
      :else n))

  (let [x 1
        1 2]
    3)


  (defmacro parse [& bodies]
    ;; Macro arguments have proper meta information:
    (println "bodies" bodies "with meta" (map meta bodies))
    ;; But just the roots of their arguments, not their children: (the symbols in the example below)
    (println "bodies children have meta" (map #(map meta %) (rest &form)))
    ;; The root of &form has meta information:
    (println "form" &form "with meta" (meta &form))
    ;; The form children have meta information again (except the first one):
    (println "form children have meta" (map meta &form))
    ;; Again the children of the form (the symbols in the example below) don't have meta information:
    (println "form grand-children" (map #(map identity %) (next &form)) "have meta" (map #(map meta %) (next &form))))

  (parse (a b) (c d))


  (parse x y z)
  (parse ((a) (b)) ((c) (d))))
