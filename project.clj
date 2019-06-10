(defproject multi-stage "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]

                 [zprint "0.4.15"] ;; TODO test only
                 [org.clojure/test.check "0.10.0-alpha3"] ;; TODO test only
                 
                 ^{:voom {:repo "https://github.com/foobar27/meliae" :branch "master"}} ;; TODO fixed version
                 [com.github.foobar27/meliae "0.1.0-20190406_160303-g7772abc"]]
  :repl-options {:init-ns multi-stage.core})
