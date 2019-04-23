(ns towers.example.serialization
  (:require [towers.clojure.generator :as clj-gen]
            [towers.ir.ast :as ir-ast]
            [towers.ir.parser :as ir-parser]
            [towers.ir.core :refer [lift run]]
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
                             ((matches (rest r)) (rest s))
                             (lift false))
                           (lift false))
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

;; TODO support loop*
;; TODO support dot forms
;; TODO support throw
(comment
  (fn write-formatted!
    [FORMAT output data]
    (let* [PRED__38802 (fn [p1__38800# p2__38801#]
                         (= p1__38800# p2__38801#))
           EXPR__38803 (::type FORMAT)]
      (if (PRED__38802 ::primitive EXPR__38803)
        (let* [PRED__38804 (fn [p1__38800# p2__38801#]
                             (= p1__38800# p2__38801#))
               EXPR__38805 (::primitive-type FORMAT)]
          (if (PRED__38804 ::int8 EXPR__38805)
            (lift (. output writeByte (int data)))
            (if (PRED__38804 ::int64 EXPR__38805)
              (lift (. output writeLong (long data)))
              (throw (new java.lang.IllegalArgumentException (str "No matching clause:"  EXPR__38805))))))
        (if (PRED__38802 ::record EXPR__38803)
          (loop* [ATTRIBUTES (::attributes FORMAT)
                  data data]
                 (if (seq ATTRIBUTES)
                   (let* [MAP__38806 (first ATTRIBUTES)
                          MAP__38806 (if (
                                          seq? map__38806)
                                       (. clojure.lang.PersistentHashMap create (seq MAP__38806))
                                       MAP__38806)
                          ATTRIBUTE-NAME (get MAP__38806 ::attribute-name)
                          ATTRIBUTE-FORMAT (get MAP__38806 ::attribute-format)]
                     (write-formatted! attribute-format output (get data ATTRIBUTE-NAME))
                     (recur (rest ATTRIBUTES) data))))
          (if (PRED__38802 ::vector EXPR__38803)
            (let* [MAP__38807 FORMAT
                   MAP__38807 (if (seq? MAP__38807)
                                (. clojure.lang.PersistentHashMap create (seq MAP__38807))
                                MAP__38807)
                   INDEX_FORMAT (get MAP__38807 ::index-format)
                   VALUE-FORMAT (get MAP__38807 ::value-format)]
              (write-formatted! INDEX-FORMAT output (count data))
              (loop* [seq_38808 (seq data)
                      chunk_38809 nil
                      count_38810 0
                      i_38811 0]
                     (if (< i_38811 count_38810)
                       (let* [item (. chunk_38809 nth i_38811)]
                         (write-formatted! VALUE-FORMAT output item)
                         (recur seq_38808 chunk_38809 count_38810 (unchecked-inc i_38811)))
                       (let* [temp__5720__auto__ (seq seq_38808)]
                         (when temp__5720__auto__
                           (let* [seq_38808 temp__5720__auto__]
                             (if (chunked-seq? seq_38808)
                               (let* [c__5983__auto__ (chunk-first seq_38808)]
                                 (recur (chunk-rest seq_38808)
                                        c__5983__auto__
                                        (int (count c__5983__auto__))
                                        (int 0)))
                               (let* [item (first seq_38808)]
                                 (write-formatted! VALUE-FORMAT output item)
                                 (recur (next seq_38808) nil 0 0)))))))))
            (throw (new java.lang.IllegalArgumentException (str "No matching clause:" EXPR__38803)))))))))

(comment
  (fn G__11132 [G__11133]
    (fn G__11134 [G__11135]
      (fn G__11136 [G__11137]
        (let* [G__11138 (fn G__11139 [G__11140]
                          (fn G__11141 [G__11142]
                            (= G__11140 G__11142)))
               G__11143 (get G__11133 ::type)]
          (if ((G__11138 ::primitive) G__11143)
            (let* [G__11144 (fn G__11145 [G__11146]
                              (fn G__11147 [G__11148]
                                (= G__11146 G__11148)))
                   G__11149 (::primitive-type G__11133)]
              (if ((G__11144 ::int8) G__11149)
                (. G__11135 writeByte (int G__11137))
                (if ((G__11144 ::int64) G__11149)
                  (. G__11135 writeLong (long G__11137))
                  (throw ((new java.lang.IllegalArgumentException (str "No matching clause:"  G__11149)) nil)))))
            (if ((G__11138 ::record) G__11143)
              (((fn G__11150 [G__11151]
                  (fn G__11152 [G__11153]
                    (if (seq G__11151)
                      (let* [G__11154 (first G__11151)
                             G__11155 (if (seq? G__11153)
                                        (. clojure.lang.PersistentHashMap create (seq G__11153))
                                        G__11153)
                             G__11156 (get G__11154 ::attribute-name)
                             G__11157 (get G__11154 ::attribute-format)]
                        (((G__11132 G__11155) G__11135) (get G__11153 G__11154))
                        ((G__11152 (rest G__11151)) G__11153)))))
                (::attributes G__11133))
               G__11137)
              (if ((G__11138 ::vector) G__11143)
                (let* [G__11158 G__11133
                       G__11159 (if (seq? G__11158)
                                  (. clojure.lang.PersistentHashMap create (seq G__11158))
                                  G__11158)
                       G__11160 (get G__11159 ::index-format)
                       G__11161 (get G__11159 ::value-format)]
                  (((G__11132 G__11159)
                    G__11135)
                   (count G__11137))
                  (((((fn G__11162 [G__11163]
                        (fn G__11164 [G__11165]
                          (fn G__11166 [G__11167]
                            (fn G__11168 [G__11169]
                              (if (< G__11168 G__11166)
                                (let* [G__11170 (. G__11164 nth G__11168)]
                                  (((G__11132 G__11160) G__11135) G__11169)
                                  ((((G__11167 G__11162)
                                     G__11164)
                                    G__11166)
                                   (unchecked-inc G__11168)))
                                (let* [G__11171 (seq G__11162)]
                                  (if G__11169
                                    (let* [G__11172 G__11169]
                                      (if (chunked-seq? G__11171)
                                        (let* [G__11173 (chunk-first G__11171)]
                                          ((((G__11167 (chunk-rest G__11171))
                                             G__11171)
                                            (int (count G__11171)))
                                           (int 0)))
                                        (let* [G__11174 (first G__11171)]
                                          (((G__11132 G__11160)
                                            G__11135)
                                           G__11171)
                                          ((((G__11167 (next G__11171))
                                             nil)
                                            0)
                                           0)))))))))))
                      (seq G__11137))
                     nil)
                    0)
                   0))
                (throw ((new java.lang.IllegalArgumentException (str "No matching clause:"  G__11143)) nil))))))))))

(comment
  (doseq [item data]
    (lift
     ((write-formatted! value-format) output item))))

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
                ::record (loop [attributes (get format ::attributes)
                                data data]
                           (if (seq attributes)
                             (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                               ((write-formatted! attribute-format) output (get data attribute-name))
                               (recur (rest attributes) data))))
                ::vector (let [{:keys [::index-format ::value-format]} format]
                           ((write-formatted! index-format) output (count data))
                           ;; TODO replace by doseq
                           ((lift (fn the-loop [data]
                                    (if (seq data)
                                      (let [[item & data] data]
                                        ((write-formatted! value-format) output item)
                                        (the-loop data)))))
                            data))))))]
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

(parse (fn read-formatted! [format input]
         (condp = (::type format)
           ;; TODO this could be a multi-method
           ::primitive (condp #(= %1 %2) (::primitive-type format)
                         ;; TODO this could be a multi-method
                         ::int8 (.readByte input)
                         ::int64 (.readLong input))
           ::record (loop [attributes (::attributes format)
                           data (lift (transient {}))]
                      ;; TODO this loop could be a reduce?
                      (if (seq attributes)
                        (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                          (recur (rest attributes)
                                 (lift (assoc! data attribute-name (read-formatted! attribute-format input)))))
                        (lift (persistent! data))))
           ::vector (let [{:keys [::index-format ::value-format]} format]
                      ;; TODO this could be a reduce
                      (loop [remaining-items (read-formatted! index-format input)
                             data (lift (transient []))]
                        (if (pos? remaining-items)
                          (recur (dec remaining-items)
                                 (lift (conj! data (read-formatted! value-format input))))
                          (persistent! data)))))))

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
                 (loop [remaining-items (read-formatted! index-format input)
                        data (transient [])]
                   (if (pos? remaining-items)
                     (lift (recur (dec remaining-items)
                                  (conj! data (read-formatted! value-format input))))
                     (lift (persistent! data))))))))
