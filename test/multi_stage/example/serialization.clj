(ns multi-stage.example.serialization
  (:require [multi-stage.clojure.generator :as clj-gen]
            [multi-stage.ir.ast :as ir-ast]
            [multi-stage.ir.parser :as ir-parser]
            [multi-stage.ir.core :refer [lift lift-loop run]]
            [multi-stage.ir.interpreter :refer [evalmsg]]
            [multi-stage.ir.generator :as ir-gen]
            [meliae.patterns :refer [print-pattern]]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument (stest/enumerate-namespace ['meliae.patterns 'multi-stage.ir.ast 'multi-stage.clojure.ast
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

(defmacro specialize [body static-arguments]
  (let [parsed-body (ir-parser/clj->ir body)
        ir (ir-ast/->run (ir-ast/->literal 0)
                         (ir-ast/->lift (ir-ast/->apply parsed-body
                                                        (vec (map #(ir-ast/->quote (eval %)) static-arguments)))))]
    (let [output (clj-gen/generate (ir-gen/generate (evalmsg [] ir) nil))]
      (println "GENERATED" output)
      output)))

(specialize
 (let [matches-generic (fn matches-generic [maybe-lift]
                         (fn rec [r]
                           (fn [s]
                             (if (seq r)
                               (if (seq s)
                                 (if (= (lift (first r))
                                        (first s))
                                   ((rec (rest r)) (rest s))
                                   (maybe-lift false))
                                 (maybe-lift false))
                               (maybe-lift true)))))
       matches-spec (matches-generic (fn [e] (lift e)))
       matches-gen (matches-generic (fn [e] e))]
   (fn [r]
     (if (< (count r) 20)
       (run 0 (lift (matches-spec r)))
       (matches-gen r))))
 ["ab"])

(def example-format-primitive
  {::type ::primitive
   ::primitive-type ::int8})

(def example-format-record
  {::type ::record
   ::attributes [{::attribute-name ::first-name
                  ::attribute-format {::type ::primitive
                                      ::primitive-type ::int8}}
                 {::attribute-name ::last-name
                  ::attribute-format {::type ::primitive
                                      ::primitive-type ::int8}}]})

(def example-format-vector
  {::type ::vector
   ::index-format example-format-primitive
   ::value-format example-format-record})

(specialize
 (fn write-formatted! [format]
   (fn [output data]
     (condp = (get format ::type)
       ;; TODO this could be a multi-method
       ::primitive (condp = (::primitive-type format)
                     ;; TODO this could be a multi-method
                     ::int8 (.writeByte output (int data)) ;; the int-cast is not a mistake, check the signature
                     ::int64 (.writeLong output (long data)))
       ::record (doseq [{:keys [::attribute-name ::attribute-format]} (::attributes format)]
                  ((write-formatted! attribute-format) output (get data attribute-name)))
       ::vector (let [{:keys [::index-format ::value-format]} format]
                  ((write-formatted! index-format) output (count data))
                  (lift-loop
                   (doseq [item data]
                     ((write-formatted! value-format) output item)))))))
 [example-format-vector])

(specialize
 (fn read-formatted! [format]
   (fn [input]
     (condp = (get format ::type)
       ::primitive (condp = (::primitive-type format)
                     ::int8 (.readByte input)
                     ::int64 (.readLong input))
       ::record (loop [attributes (::attributes format)
                       data (transient (lift {}))]
                  (if (seq attributes)
                    (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                      (recur (rest attributes)
                             (assoc! data attribute-name ((read-formatted! attribute-format) input))))
                    (persistent! data)))
       ::vector (let [{:keys [::index-format ::value-format]} format]
                  (lift-loop
                   (loop [remaining-items ((read-formatted! index-format) input)
                          data (transient (lift []))]
                     (if (pos? remaining-items)
                       (recur (dec remaining-items)
                              (conj! data ((read-formatted! value-format) input)))
                       (persistent! data))))))))
 [example-format-vector])
