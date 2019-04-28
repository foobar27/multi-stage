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

(->call
 (->fn* G__20305 ({:args [G__20306],
                   :bodies [#multi-stage.clojure.ast.Expression{:meliae.patterns/kind :multi-stage.clojure.ast/call, :multi-stage.clojure.ast/function #multi-stage.clojure.ast.Expression{:meliae.patterns/kind :multi-stage.clojure.ast/variable, :multi-stage.clojure.ast/symbol G__20305}, :multi-stage.clojure.ast/arguments [#multi-stage.clojure.ast.Expression{:meliae.patterns/kind :multi-stage.clojure.ast/invoke, :multi-stage.clojure.ast/function #multi-stage.clojure.ast.Expression{:meliae.patterns/kind :multi-stage.clojure.ast/literal, :multi-stage.clojure.ast/value clojure.core/inc}, :multi-stage.clojure.ast/args (#multi-stage.clojure.ast.Expression{:meliae.patterns/kind :multi-stage.clojure.ast/variable, :multi-stage.clojure.ast/symbol G__20306}), :multi-stage.clojure.ast/tail-call? true}]}]}))
 [(->literal 1)])
