(defproject towers "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-voom "0.1.0-20180617_140646-g0ba7ec8"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 ^{:voom {:repo "https://github.com/foobar27/meliae" :branch "master"}} ;; TODO fixed version
                 [foobar27/meliae "0.1.0"]
                 ]
  :repl-options {:init-ns towers.core})
