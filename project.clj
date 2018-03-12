(defproject mlearning "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options { :timeout 120000 }
  :dependencies [;[proto-repl "0.3.1"]
                 ;[proto-repl-charts "0.3.2"]
                 [yieldbot/vizard "1.0.1"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot mlearning.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
