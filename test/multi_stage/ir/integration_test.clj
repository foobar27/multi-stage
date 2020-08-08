(ns multi-stage.ir.integration-test
  (:require [multi-stage.ir.core :refer [lift run]]
            [multi-stage.ir.interpreter :as interpreter]
            [multi-stage.test-utils :refer [verify-pattern remove-gensym]]
            [multi-stage.pre.ast :as pre-ast]
            [multi-stage.post.ast]
            [multi-stage.core :as ms]
            [multi-stage.ir.parser :as ir-parser]
            [multi-stage.impl.core :as impl]
            [meliae.patterns :refer [print-pattern]]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [multi-stage.test-utils :refer [specialize]]))
(stest/instrument
 (stest/enumerate-namespace
  ['meliae.patterns
   'multi-stage.ir.ast
   'multi-stage.ir.value
   'multi-stage.post.ast]))

;;
;; No argument, constant function.
;;

(comment
  (multi-stage.impl.core/reset-definitions!)

  (multi-stage.impl.core/determine-all-registered-variables)
  
  (multi-stage.impl.core/print-variable-definition `plus)
  (multi-stage.impl.core/print-variable-definition `sum-1-to-n)

  (multi-stage.impl.core/symbol->symbols-to-recompile `plus)
  )


(ms/defn return-42 []
  42)

(ms/compile return-42)

(deftest test-return-42
  (testing
      (is (= 42 (return-42)))))

;;
;; Two-argument function
;;

(ms/defn plus [x y]
  (+ x y))

(ms/compile plus)

(deftest test-plus
  (testing
      (is (= 3 (plus 1 2)))))

;;
;; Adding numbers from 1 to n
;; (reusing the function plus which was defined above)
;;

(ms/defn sum-1-to-n [n]
  (loop [n n, sum 0]
    (if (> n 0)
      (recur (dec n) (plus sum n))
      sum)))

(ms/compile sum-1-to-n)

(deftest test-sum-1-to-n
  (testing
      (is (= 55 (sum-1-to-n 10)))))


;;
;; Prefix matching for strings
;;

(ms/defn starts-with-simple [pattern]
  (fn [string]
    (if (seq pattern)
      (if (seq string)
        (if (= (lift (first pattern))
               (first string))
          ((starts-with-simple (rest pattern)) (rest string))))
      (lift true))))

(ms/compile starts-with-simple)

;; TODO ms/defn- is buggy
(ms/defn starts-with-polymorphic [maybe-lift]
  (fn rec [prefix]
    (fn [string]
      (if (seq prefix)
        (if (seq string)
          (if (= (lift (first prefix))
                 (first string))
            ((rec (rest prefix)) (rest string))
            (maybe-lift false))
          (maybe-lift false))
        (maybe-lift true)))))

(ms/def ^:private starts-with-specialized
  (starts-with-polymorphic (fn [e] (lift e))))

(ms/def ^:private starts-with-generic
  (starts-with-polymorphic (fn [e] e)))


(ms/compile starts-with-generic)

(ms/defn starts-with-optimized [pattern string]
  (if (< (count pattern) 20)
    (run 0 (lift (starts-with-specialized pattern)))
    (starts-with-generic pattern)))

(ms/def foo-ab
  (run 0 (lift (starts-with-specialized "ab"))))

(let [variable (impl/get-registered-global-variable *ns* `foo-ab)
      definition (impl/variable->definition variable)
      dependencies (for [variable (drop-last (impl/variable->sorted-dependencies variable))]
                     [variable (impl/variable->definition variable)])
      pre (reduce (fn [expression [variable variable-definition]]
                    (pre-ast/->let (::pre-ast/source-context expression)
                                   variable
                                   variable-definition
                                   expression))
                  definition
                  (reverse dependencies))
      ir (ir-parser/pre->ir pre {})
      ir (interpreter/evalmsg [] ir 'foo-ab)
      ;; TODO ir-gen/generate
      ;; TODO clj-gen/generate
      ]
  (print-pattern ir))

(ms/def starts-with-ab-simple
  ;; TODO this would mean that we provide IR code where PRE code is expected!
  (partial-lift starts-with-simple "ab"))

(ms/def starts-with-ab
  (partial-lift starts-with-optimized "ab"))

;;
;; TODO remove
;; 

(def start-ab-simple
  (specialize
   (fn matches [r]
     (fn matches-inner [s]
       (if (seq r)
         (if (seq s)
           (if (= (lift (first r))
                  (first s))
             ((matches (rest r)) (rest s))))
         (lift true))))
   ["ab"]))

(def start-ab-optimized
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
   ["ab"]))

(deftest start-ab-test
  (are [input result]
      (and (= (boolean result) (boolean (start-ab-simple input)))
           (= (boolean result) (boolean (start-ab-optimized input))))
    "" false
    "a" false
    "ab" true
    "abc" true
    "acb" false))

(comment
  (specialize
   (let [maybe-lift (fn [e] e)]
     (let [star-loop (fn star-loop [match-pattern]
                       (fn [c]
                         (maybe-lift (fn inner-loop [string]
                                       (if (= (maybe-lift :yes) (match-pattern string))
                                         (maybe-lift :yes)
                                         (let [[s & string] string]
                                           (if (= (maybe-lift :done) s)
                                             (maybe-lift :no)
                                             (if (= '_ c)
                                               (inner-loop string)
                                               (if (= (maybe-lift c) s)
                                                 (inner-loop string)
                                                 (maybe-lift :no))))))))))]
       (let [match-here (fn match-here [[p & pattern]]
                          (fn [string]
                            (if (= :done p)
                              (maybe-lift :yes)
                              (let [m (fn [[s & string]]
                                        (if (= '_ p)
                                          (if (= (maybe-lift :done) s)
                                            (maybe-lift :no)
                                            ((match-here pattern) string))
                                          (if (= (maybe-lift :done) s)
                                            (maybe-lift :no)
                                            (if (= (maybe-lift p) s)
                                              ((match-here pattern) string)
                                              (maybe-lift :no)))))]
                                (let [[p & pattern] pattern]
                                  (if (= :done p)
                                    (m string)
                                    (if (= '* p)
                                      (((star-loop (match-here pattern))
                                        p)
                                       string)
                                      (m string))))))))]
         (let [match (fn match [pattern]
                       (if (= :done (first pattern))
                         (maybe-lift (fn [string]
                                       (maybe-lift :yes)))
                         (maybe-lift (match-here pattern))))]
           match))))
   ['(_ a _ * :done)]))


(comment
  (def matcher-example01
    `((let [maybe-lift (fn [e] e)]
        ~@matcher-src)
      '(_ a  _ * :done)
      '(b a :done)))

  (evalms (list `(let maybe-lift (lambda _ e e) ,matcher-src)
                `(_ * a _ * done)
                `(b a done))
          `((((,pink-eval-exp3 (var 0))
              nil-env)
             (var 1))
            (var 2))))

