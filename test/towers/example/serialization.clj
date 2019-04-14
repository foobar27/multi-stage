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

(fn* G__22863 [G__22864]
     (fn* G__22865 [G__22866]
          (fn* G__22867 [G__22868]
               (let* [G__22869 (fn* G__22870 ([G__22871] (fn* G__22872 ([G__22873] ((quote clojure.core/=) G__22871 G__22873))))) G__22874 ((quote clojure.core/get) G__22864 :towers.example.serialization/type)]
                 (if ((G__22869 :towers.example.serialization/primitive) G__22874)
                   (let* [G__22875 (fn* G__22876 ([G__22877]
                                                  (fn* G__22878 ([G__22879]
                                                                 ((quote clojure.core/=) G__22877 G__22879)))))
                          G__22880 (:towers.example.serialization/primitive-type G__22864)]
                     (if ((G__22875 :towers.example.serialization/int8) G__22880)
                       (. #towers.clojure.ast.Expression{:meliae.patterns/kind :towers.clojure.ast/variable, :towers.clojure.ast/symbol G__22866} writeByte ((quote clojure.core/int) G__22868))
                       (if ((G__22875 :towers.example.serialization/int64) G__22880)
                         (. #towers.clojure.ast.Expression{:meliae.patterns/kind :towers.clojure.ast/variable, :towers.clojure.ast/symbol G__22866} writeLong ((quote clojure.core/long) G__22868))
                         (throw ((new java.lang.IllegalArgumentException
                                      ((quote clojure.core/str) No matching clause:  G__22880)) (quote nil))))))
                   (if ((G__22869 :towers.example.serialization/record) G__22874)
                     (((fn* G__22881 [G__22882]
                            (fn* G__22883 [G__22884]
                                 (if ((quote clojure.core/seq) G__22882)
                                   (let* [G__22885 ((quote clojure.core/first) G__22882)
                                          G__22886 (if ((quote clojure.core/seq?) G__22884) (. #towers.clojure.ast.Expression{:meliae.patterns/kind :towers.clojure.ast/class-reference, :towers.clojure.ast/class-name clojure.lang.PersistentHashMap} create ((quote clojure.core/seq) G__22884)) G__22884)
                                          G__22887 ((quote clojure.core/get) G__22885 :towers.example.serialization/attribute-name)
                                          G__22888 ((quote clojure.core/get) G__22885 :towers.example.serialization/attribute-format)]
                                     ;; TODO should have been removed by smart-let
                                     (do (((G__22863 G__22886) G__22866) ((quote clojure.core/get) G__22884 G__22885))
                                         ((G__22883 ((quote clojure.core/rest) G__22882)) G__22884))))))
                       (:towers.example.serialization/attributes G__22864))
                      G__22868)
                     (if ((G__22869 :towers.example.serialization/vector) G__22874)
                       (let* [G__22889 G__22864
                              G__22890 (if ((quote clojure.core/seq?) G__22889)
                                         (. #towers.clojure.ast.Expression{:meliae.patterns/kind :towers.clojure.ast/class-reference, :towers.clojure.ast/class-name clojure.lang.PersistentHashMap} create ((quote clojure.core/seq) G__22889))
                                         G__22889)
                              G__22891 ((quote clojure.core/get) G__22890 :towers.example.serialization/index-format)
                              G__22892 ((quote clojure.core/get) G__22890 :towers.example.serialization/value-format)]
                         ;; TODO should have been removed by smart-let
                         (do (((G__22863 G__22890) G__22866) ((quote clojure.core/count) G__22868))
                             (((((fn* G__22893 ([G__22894] (fn* G__22895 [G__22896]
                                                                (fn* G__22897 [G__22898]
                                                                     (fn* G__22899 [G__22900]
                                                                          (if ((quote clojure.core/<) G__22899 G__22897)
                                                                            (let* [G__22901 (. #towers.clojure.ast.Expression{:meliae.patterns/kind :towers.clojure.ast/variable, :towers.clojure.ast/symbol G__22895} nth G__22899)]
                                                                              (((G__22863 G__22891) G__22866) G__22900)
                                                                              ((((G__22898 G__22893) G__22895) G__22897)
                                                                               ((quote clojure.core/unchecked-inc) G__22899)))
                                                                            (let* [G__22902 ((quote clojure.core/seq) G__22893)]
                                                                              (if G__22900 (let* [G__22903 G__22900]
                                                                                             (if ((quote clojure.core/chunked-seq?) G__22902)
                                                                                               (let* [G__22904 ((quote clojure.core/chunk-first) G__22902)]
                                                                                                 ((((G__22898 ((quote clojure.core/chunk-rest) G__22902)) G__22902) ((quote clojure.core/int) ((quote clojure.core/count) G__22902))) ((quote clojure.core/int) 0))) (let* [G__22905 ((quote clojure.core/first) G__22902)] (((G__22863 G__22891) G__22866) G__22902) ((((G__22898 ((quote clojure.core/next) G__22902)) (quote nil)) 0) 0)))))))))))) ((quote clojure.core/seq) G__22868)) (quote nil)) 0) 0))) (throw ((new java.lang.IllegalArgumentException ((quote clojure.core/str) No matching clause:  G__22874)) (quote nil))))))))))

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
  (println "IR=")
  (meliae.patterns/print-pattern ir)
  (println)
  (println "FINAL RESULT")
  (println (clj-gen/generate (ir-gen/generate ir nil))))

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
