(ns multi-stage.example.serialization
  (:require [multi-stage.post.generator :as clj-gen]
            [multi-stage.ir.ast :as ir-ast]
            [multi-stage.ir.parser :as ir-parser]
            [multi-stage.ir.core :refer [lift lift-loop run]]
            [multi-stage.ir.interpreter :refer [evalmsg]]
            [multi-stage.ir.generator :as ir-gen]
            [meliae.patterns :refer [print-pattern]]
            [clojure.spec.test.alpha :as stest]
            [multi-stage.test-utils :refer [specialize]]
            ))

(stest/instrument
 (stest/enumerate-namespace ['meliae.patterns
                             'multi-stage.ir.ast
                             'multi-stage.ir.value
                             'multi-stage.post.ast
                             ]))

 ;; TODO additional features:
 ;; TODO  - multimethods
 ;; TODO  - hashmaps are functions too
 ;; TODO  - some loops could be replaced by reduce
 ;; TODO partial application of functions

 ;; TODO Replace :done by seq-check?
 ;; TODO introduce loop-recur

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

;; TODO How do we know which file is not ascending?
(defn ensure-ascending [get-key smaller? xs]
  (let [previous-key (volatile! ::none)]
    (lazy-seq
     (if-let [xs (seq xs)]
       (let [x (first xs)
             key (get-key x)
             xs (rest xs)]
         (if (smaller? previous-key key)
           (do
             (vreset! previous-key key)
             (cons key (ensure-ascending get-key smaller? xs)))
           (throw (IllegalArgumentException. "Ascending keys!"))))))))

 (defn remove-expired [expired? input]
   (remove expired? input))

 ;; TODO can we optimize this? like two nested loops
 (defn remove-consecutive-duplicates [equals? newer? xs]
   (letfn [(recurse [xs]
             (remove-consecutive-duplicates equals? newer? xs))]
     (lazy-seq
      (if-let [xs (seq xs)]
        (let [x (first xs)]
          (if-let [xs (seq (rest xs))]
            (let [y (first xs)
                  xs (rest xs)]
              (if (equals? x y)
                (if (newer? x y)
                  (recurse (cons x xs))
                  (recurse (cons y xs)))
                (cons x (recurse (cons y xs)))))
            (cons x nil)))))))

 ;; TODO how do we take care that the values are not all in memory until the last moment when needed?
 (defn merge-2 [smaller? xs ys]
   (lazy-seq
    (if-let [xs (seq xs)]
      (if-let [ys (seq ys)]
        (let [x (first xs)
              y (first ys)]
          (if (smaller? x y)
            (cons x (merge-2 smaller? (rest xs) ys))
            (cons y (merge-2 smaller? xs (rest ys)))))
        ;;  ys empty
        xs)
      ;; xs empty
      ys)))

 (comment
   (defn merge-ascending [smaller? xss]
     (condp = (count xss)
       0 nil
       1 (first xss)
       ;; default: at least 2
       (let [[xs ys & xss] xss]
         (merge-n smaller? (cons (merge-2 smaller? xs ys) xss))))))


 ;; TODO when one file is not ascending, try again without this file

(comment
  (defn compact [xss]
    (let [get-key ::key
          get-version ::version
          smaller? #(< (get-key %1)
                       (get-key %2))
          newer? #(> (get-version %1)
                     (get-version %2))
          equals? #(= (get-key %1)
                      (get-key %2))]
      (->> (merge-ascending smaller?
                            (map #(->> %
                                       (ensure-ascending get-key <)
                                       (remove-expired equals? newer?))
                                 xs))
           (remove-consecutive-duplicates equals?)))))

 ;; Continuation passing style

 (comment
   (let [numbers [0 1 2 3 4 5]]
     ((fn print-all [xs]
        (when-let [xs (seq xs)]
          (println (first xs))
          (print-all (rest xs))))
      ((fn my-generator [xs]
         (LazySeq.
          (fn my-map []
            (when-let [xs (seq xs)]
              (let [x (first xs)]
                (cons (dec x)
                      (cons (inc x)
                            (my-map (rest xs)))))))))
       numbers))))


 ;; condition: generator must return cons-cell

 (fn print-all [xs]
   (when-let [xs (seq xs)]
     (when-let [xs (seq xs)]
       (println (dec (first xs))))
     (when-let [xs (seq xs)]
       (println (inc (first xs))))
     (print-all (rest xs))))

 (comment
   TODO filter
   TODO duplicate
   TODO chunked sequences)


 (comment
   (fn my-map-c [c]
     (when-let [xs (seq xs)]
       (let [x (first xs)]
         (c (cons (dec x)
                  (cons (inc x)
                        )))))))


 (defn depth [tree]
   (if (int? tree)
     0
     (let [[left right] tree
           depth-left (depth left)
           depth-right (depth right)]
       (inc (max depth-left depth-right)))))


 (defn depth-CPS [tree k]
   (if (int? tree)
     (k 0)
     (let [[left right] tree]
       (depth left
              (fn [depth-left]
                (depth right
                       (fn [depth-right]
                         (k (inc (max depth-left depth-right))))))))))

(comment
  (depth [1 [2 [3 4]]]))
