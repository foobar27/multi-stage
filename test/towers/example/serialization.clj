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


(ir-parser/parse
 (fn write-formatted! [format output data]
   (condp #(= %1 %2) (::type format)
     ;; TODO this could be a multi-method
     ::primitive (condp = (::primitive-type format)
                   ;; TODO this could be a multi-method
                   ::int8 (lift (.writeByte output (int data))) ;; the int-cast is not a mistake, check the signature
                   ::int64 (lift (.writeLong output (long data))))
     ::record (loop [attributes (::attributes format)
                     data data]
                (when (seq attributes)
                  (let [{:keys [::attribute-name ::attribute-format]} (first attributes)]
                    (write-formatted! attribute-format output (get data attribute-name))
                    (recur (rest attributes) data))))
     ::vector (let [{:keys [::index-format ::value-format]} format]
                (write-formatted! index-format output (count data))
                (doseq [item data]
                  (write-formatted! value-format output item))))))

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
