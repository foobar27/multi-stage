(ns multi-stage.ir.integration-test
  (:require [multi-stage.clojure.generator :as clj-gen]
            [multi-stage.ir.generator :as ir-gen]
            [multi-stage.ir.ast :as ir-ast :refer :all]
            [multi-stage.ir.parser :as ir-parser :refer [parse]]
            [multi-stage.ir.core :refer [lift run]]
            [multi-stage.test-utils :refer [verify-pattern remove-auto-gensym]]
            [meliae.patterns :refer [print-pattern]]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument (stest/enumerate-namespace ['meliae.patterns 'multi-stage.ir.ast 'multi-stage.clojure.ast]))

(meliae.patterns/print-pattern (ir-gen/generate (parse (if 1 2 3)) nil))

(meliae.patterns/print-pattern (ir-gen/generate (parse (let [x 1] (loop [x x] (recur (inc x))))) nil))

