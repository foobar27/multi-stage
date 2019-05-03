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
  (let [ir (ir-parser/clj->ir `(run 0 (lift (~body
                                             ~@(for [arg static-arguments]
                                                 `(quote ~(eval arg)))))))
        parsed-body (ir-parser/clj->ir body)
        ir (ir-ast/->run (ir-ast/->literal 0)
                         (ir-ast/->lift (ir-ast/->apply parsed-body
                                                        (vec (map #(ir-ast/->quote (eval %)) static-arguments)))))]
    (let [output (clj-gen/generate (ir-gen/generate (evalmsg [] ir) nil))]
      (println "GENERATED" output)
      (comment output))))

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

(->let (->lambda 2
                 (->primitive-call = ((->variable 6) (->variable 7)))
                 unnamed16864
                 [p1__16824# p2__16825#])
       (->let (->primitive-call get ((->variable 1) (->literal :multi-stage.example.serialization/type)))
              (->if (->apply (->variable 5) ((->literal :multi-stage.example.serialization/primitive) (->variable 6)))
                    (->let (->lambda 2
                                     (->primitive-call = ((->variable 8) (->variable 9)))
                                     unnamed16865
                                     [p1__16826# p2__16827#])
                           (->let (->primitive-call get ((->variable 1) (->literal :multi-stage.example.serialization/primitive-type)))
                                  (->if (->apply (->variable 7)
                                                 ((->literal :multi-stage.example.serialization/int8)
                                                  (->variable 8)))
                                        (->dot (->variable 3) writeByte ((->primitive-call int ((->variable 4)))))
                                        (->if (->apply (->variable 7)
                                                       ((->literal :multi-stage.example.serialization/int64)
                                                        (->variable 8)))
                                              (->dot (->variable 3) writeLong ((->primitive-call long ((->variable 4)))))
                                              (->throw (->new java.lang.IllegalArgumentException ((->primitive-call str ((->literal No matching clause: ) (->variable 8))))))))
                                  expr__16851)
                           pred__16850)
                    (->if (->apply (->variable 5) ((->literal :multi-stage.example.serialization/record)
                                                   (->variable 6)))
                          (->apply (->lambda 4 (->if (->primitive-call < ((->variable 11) (->variable 10)))
                                                     (->let (->dot (->variable 9) nth ((->variable 11)))
                                                            (->let (->if (->primitive-call seq? ((->variable 12)))
                                                                         (->dot (->class-reference clojure.lang.PersistentHashMap) create ((->primitive-call seq ((->variable 12)))))
                                                                         (->variable 12))
                                                                   (->let (->primitive-call get ((->variable 13) (->literal :multi-stage.example.serialization/attribute-name)))
                                                                          (->let (->primitive-call get ((->variable 13) (->literal :multi-stage.example.serialization/attribute-format)))
                                                                                 (->do ((->apply (->apply (->variable 0) ((->variable 15)))
                                                                                                 ((->variable 3)
                                                                                                  (->primitive-call get ((->variable 4) (->variable 14)))))
                                                                                        (->apply (->variable 7)
                                                                                                 ((->variable 8)
                                                                                                  (->variable 9)
                                                                                                  (->variable 10)
                                                                                                  (->primitive-call unchecked-inc ((->variable 11)))))))
                                                                                 attribute-format)
                                                                          attribute-name)
                                                                   map__16856)
                                                            map__16856)
                                                     (->let (->primitive-call seq ((->variable 8)))
                                                            (->if (->variable 12)
                                                                  (->let (->variable 12)
                                                                         (->if (->primitive-call chunked-seq? ((->variable 13)))
                                                                               (->let (->primitive-call chunk-first ((->variable 13)))
                                                                                      (->apply (->variable 7)
                                                                                               ((->primitive-call chunk-rest ((->variable 13)))
                                                                                                (->variable 14)
                                                                                                (->primitive-call int ((->primitive-call count ((->variable 14)))))
                                                                                                (->primitive-call int ((->literal 0)))))
                                                                                      c__5983__auto__)
                                                                               (->let (->primitive-call first ((->variable 13)))
                                                                                      (->let (->if (->primitive-call seq? ((->variable 14)))
                                                                                                   (->dot (->class-reference clojure.lang.PersistentHashMap) create ((->primitive-call seq ((->variable 14)))))
                                                                                                   (->variable 14))
                                                                                             (->let (->primitive-call get ((->variable 15) (->literal :multi-stage.example.serialization/attribute-name)))
                                                                                                    (->let (->primitive-call get ((->variable 15) (->literal :multi-stage.example.serialization/attribute-format)))
                                                                                                           (->do ((->apply (->apply (->variable 0)
                                                                                                                                    ((->variable 17)))
                                                                                                                           ((->variable 3)
                                                                                                                            (->primitive-call get ((->variable 4) (->variable 16)))))
                                                                                                                  (->apply (->variable 7)
                                                                                                                           ((->primitive-call next ((->variable 13)))
                                                                                                                            (->literal nil)
                                                                                                                            (->literal 0)
                                                                                                                            (->literal 0)))))
                                                                                                           attribute-format)
                                                                                                    attribute-name)
                                                                                             map__16857)
                                                                                      map__16857))
                                                                         seq_16852)
                                                                  (->literal nil))
                                                            temp__5720__auto__))
                                             loop16866
                                             [seq_16852 chunk_16853 count_16854 i_16855])
                                   [(->primitive-call seq ((->primitive-call get ((->variable 1) (->literal :multi-stage.example.serialization/attributes)))))
                                    (->literal nil)
                                    (->literal 0)
                                    (->literal 0)])
                          (->if (->apply (->variable 5)
                                         ((->literal :multi-stage.example.serialization/vector)
                                          (->variable 6)))
                                (->let (->variable 1)
                                       (->let (->if (->primitive-call seq? ((->variable 7)))
                                                    (->dot (->class-reference clojure.lang.PersistentHashMap) create ((->primitive-call seq ((->variable 7)))))
                                                    (->variable 7))
                                              (->let (->primitive-call get ((->variable 8) (->literal :multi-stage.example.serialization/index-format)))
                                                     (->let (->primitive-call get ((->variable 8) (->literal :multi-stage.example.serialization/value-format)))
                                                            (->do ((->apply (->apply (->variable 0)
                                                                                     ((->variable 9)))
                                                                            ((->variable 3)
                                                                             (->primitive-call count ((->variable 4)))))
                                                                   (->apply (->lift (->lambda 4
                                                                                              (->if (->primitive-call < ((->variable 15) (->variable 14)))
                                                                                                    (->let (->dot (->variable 13) nth ((->variable 15)))
                                                                                                           (->do ((->apply (->apply (->variable 0)
                                                                                                                                    ((->variable 10)))
                                                                                                                           ((->variable 3)
                                                                                                                            (->variable 16)))
                                                                                                                  (->apply (->variable 11)
                                                                                                                           ((->variable 12)
                                                                                                                            (->variable 13)
                                                                                                                            (->variable 14)
                                                                                                                            (->primitive-call unchecked-inc ((->variable 15)))))))
                                                                                                           item)
                                                                                                    (->let (->primitive-call seq ((->variable 12)))
                                                                                                           (->if (->variable 16)
                                                                                                                 (->let (->variable 16)
                                                                                                                        (->if (->primitive-call chunked-seq? ((->variable 17)))
                                                                                                                              (->let (->primitive-call chunk-first ((->variable 17)))
                                                                                                                                     (->apply (->variable 11)
                                                                                                                                              ((->primitive-call chunk-rest ((->variable 17)))
                                                                                                                                               (->variable 18)
                                                                                                                                               (->primitive-call int ((->primitive-call count ((->variable 18)))))
                                                                                                                                               (->primitive-call int ((->literal 0)))))
                                                                                                                                     c__5983__auto__)
                                                                                                                              (->let (->primitive-call first ((->variable 17)))
                                                                                                                                     (->do ((->apply (->apply (->variable 0)
                                                                                                                                                              ((->variable 10)))
                                                                                                                                                     ((->variable 3)
                                                                                                                                                      (->variable 18)))
                                                                                                                                            (->apply (->variable 11)
                                                                                                                                                     ((->primitive-call next ((->variable 17)))
                                                                                                                                                      (->literal nil)
                                                                                                                                                      (->literal 0)
                                                                                                                                                      (->literal 0)))))
                                                                                                                                     item))
                                                                                                                        seq_16859)
                                                                                                                 (->literal nil))
                                                                                                           temp__5720__auto__))
                                                                                              loop16867
                                                                                              [seq_16859 chunk_16860 count_16861 i_16862]))
                                                                            [(->primitive-call seq ((->variable 4)))
                                                                             (->literal nil)
                                                                             (->literal 0)
                                                                             (->literal 0)])))
                                                            value-format)
                                                     index-format)
                                              map__16858)
                                       map__16858)
                                (->throw (->new java.lang.IllegalArgumentException ((->primitive-call str ((->literal "No matching clause:") (->variable 6)))))))))
              expr__16849)
       pred__16848)

(specialize
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
                               ((write-formatted! value-format) output item)))))))
 [example-format-vector])

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
