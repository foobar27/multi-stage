(ns towers.example.serialization
  (:require [towers.clojure.generator :as clj-gen]
            [towers.ir.ast :as ir-ast]
            [towers.ir.parser :as ir-parser]
            [towers.ir.core :refer [lift lift-loop run]]
            [towers.ir.interpreter :refer [evalmsg]]
            [towers.ir.generator :as ir-gen]
            [meliae.patterns :refer [print-pattern]]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument (stest/enumerate-namespace ['meliae.patterns 'towers.ir.ast 'towers.clojure.ast
                                              ]))

;; TODO how do we lift loops?
;; TODO tail call elimination

;; TODO additional features:
;; TODO  - multimethods
;; TODO  - hashmaps are functions too
;; TODO  - some loops could be replaced by reduce
;; TODO partial application of functions

(def matches
  (ir-parser/parse (fn matches [r]
                     (fn matches-inner [s]
                       (if (seq r)
                         (if (seq s)
                           (if (= (lift (first r))
                                  (first s))
                             ((matches (rest r)) (rest s))))
                         (lift true))))))

(meliae.patterns/print-pattern matches)

(print-pattern
 (evalmsg [] (ir-ast/->run (ir-ast/->literal 0) (ir-ast/->lift (ir-ast/->apply matches [(ir-ast/->quote '(a b))])))))

(clojure.pprint/pprint
 (clj-gen/generate
  (ir-gen/generate
   (evalmsg []
            (ir-ast/->run (ir-ast/->literal 0) (ir-ast/->lift (ir-ast/->apply matches [(ir-ast/->quote '(a b))]))))
   nil)))

(let [ir (ir-parser/parse
          (fn write-formatted! [format]
            (fn [output data]
              ;; TODO use = as lambda reference
              (condp #(= %1 %2) (get format ::type)
                ;; TODO this could be a multi-method
                ::primitive (condp #(= %1 %2) (get format ::primitive-type)
                              ;; TODO this could be a multi-method
                              ::int8 (.writeByte output (int data)) ;; the int-cast is not a mistake, check the signature
                              ::int64 (.writeLong output (long data)))
                ::record (doseq [{:keys [::attribute-name ::attribute-format]} (get format ::attributes)]
                           ((write-formatted! attribute-format) output (get data attribute-name)))
                ::vector (let [{:keys [::index-format ::value-format]} format]
                           ((write-formatted! index-format) output (count data))
                           (lift-loop (doseq [item data]
                                        ((write-formatted! value-format) output item))))))))]
  (println "IR=")
  (meliae.patterns/print-pattern ir)
  (println)

  (let [primitive-format {::type ::primitive
                          ::primitive-type ::int8}
        record-format {::type ::record
                       ::attributes [{::attribute-name ::first-name
                                      ::attribute-format {::type ::primitive
                                                          ::primitive-type ::int8}}
                                     {::attribute-name ::last-name
                                      ::attribute-format {::type ::primitive
                                                          ::primitive-type ::int8}}]}
        vector-format {::type ::vector
                       ::index-format primitive-format
                       ::value-format record-format}
        format vector-format
        ir (evalmsg [] (ir-ast/->run (ir-ast/->literal 0)
                                     (ir-ast/->lift (ir-ast/->apply ir [(ir-ast/->quote format)]))))]
    
    (println "INTERPRETED=")
    (meliae.patterns/print-pattern ir)
    (println)
    
    
    (let [clj-ast (ir-gen/generate ir nil)]
      
      (println "CLJ AST:")
      (meliae.patterns/print-pattern clj-ast)
      (println)
      
      (println "FINAL RESULT")
      (println (clj-gen/generate clj-ast))
      (println))))

(defn write-formatted! [format]
  (fn [output data]
    (condp = (get format ::type)
      ::primitive (condp = (get format ::primitive-type)
                    ::int8 (lift (.writeByte output (int data))) ;; the int-cast is not a mistake, check the signature
                    ::int64 (lift (.writeLong output (long data))))
      ::record (loop [attributes (get format ::attributes)
                      data (lift data)]
                 (when (seq attributes)
                   (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                     (lift (write-formatted! attribute-format output (get data attribute-name)))
                     (recur (rest attributes) (lift data)))))
      ::vector (let [{:keys [::index-format ::value-format]} format]
                 (lift (write-formatted! index-format output (count data)))
                 (doseq [item (lift data)]
                   (lift (write-formatted! value-format output item)))))))

(defn read-formatted! [format]
  (fn [input]
    (condp = (get format ::type)
      ::primitive (condp = (get format ::primitive-type)
                    ::int8 (lift (.readByte input))
                    ::int64 (lift (.readLong input)))
      ::record (loop [attributes (::attributes format)
                      data (transient {})]
                 (if (seq attributes)
                   (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                     (recur (rest attributes)
                            (assoc! data attribute-name (read-formatted! attribute-format input))))
                   (lift (persistent! data))))
      ::vector (let [{:keys [::index-format ::value-format]} format]
                 (lift-loop
                  (loop [remaining-items (read-formatted! index-format input)
                         data (transient [])]
                    (if (pos? remaining-items)
                      (recur (dec remaining-items)
                             (conj! data (read-formatted! value-format input)))
                      (persistent! data))))))))
