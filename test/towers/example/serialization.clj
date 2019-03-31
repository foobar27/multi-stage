(ns towers.example.serialization
  (:require [towers.clojure.generator :as clj-gen]
            [towers.ir.ast :as ir-ast]
            [towers.ir.parser :as ir-parser]
            [towers.ir.core :refer [lift run]]
            [towers.ir.interpreter :refer [evalmsg]]
            [towers.ir.generator :as ir-gen]
            [meliae.patterns :refer [print-pattern]]))

;; TODO understand loop-recur
;; TODO how do we lift loops?
;; TODO tail call elimination

;; TODO additional features:
;; TODO  - multimethods
;; TODO  - hashmaps are functions too
;; TODO  - some loops could be replaced by reduce
;; TODO partial application of functions

(def matches
  (ir-parser/parse (fn matches [r s]
                     (if (seq r)
                       (if (seq s)
                         (if (= (lift (first r))
                                (first s))
                           (matches (rest r) (rest s))
                           (lift false))
                         (lift false))
                       (lift true)))))

(meliae.patterns/print-pattern matches)

(print-pattern (evalmsg [] (ir-ast/->run (ir-ast/->literal 0) (ir-ast/->lift (ir-ast/->apply matches (ir-ast/->quote '(a b)))))))


(clojure.pprint/pprint
 (clj-gen/generate
  (ir-gen/generate
   (evalmsg []
            (ir-ast/->run (ir-ast/->literal 0) (ir-ast/->lift (ir-ast/->apply matches (ir-ast/->quote '(a b))))))
   nil)))

;; TODO support loop*
;; TODO support dot forms
;; TODO support throw
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
                        MAP__38806 (if (seq? map__38806)
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
          (throw (new java.lang.IllegalArgumentException (str "No matching clause:" EXPR__38803))))))))

(let [ir (ir-parser/parse
          (fn write-formatted! [format output data]
            (condp #(= %1 %2) (get format ::type)
              ;; TODO this could be a multi-method
              ::primitive (condp #(= %1 %2) (::primitive-type format)
                            ;; TODO this could be a multi-method
                            ::int8 (.writeByte output (int data)) ;; the int-cast is not a mistake, check the signature
                            ::int64 (.writeLong output (long data)))
              ::record (loop [attributes (::attributes format)
                              data data]
                         (when (seq attributes)
                           (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                             (write-formatted! attribute-format output (get data attribute-name))
                             (recur (rest attributes) data))))
              ::vector (let [{:keys [::index-format ::value-format]} format]
                         (write-formatted! index-format output (count data))
                         (doseq [item data]
                           (write-formatted! value-format output item))))))]
  (clj-gen/generate (ir-gen/generate ir nil)))

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
                          (lift (persistent! data))))))))

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
