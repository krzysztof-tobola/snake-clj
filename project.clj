(defproject
  snake "0.1.0-SNAPSHOT"
  :description "snake"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]
                 [org.craigandera/dynne "0.4.1"]]
  :main snake.launcher
  :profiles {:uberjar {:aot :all}}
  :source-paths ["src/clj" "src/cljc"]
  :clj {:builds [{:source-paths ["src/clj" "src/cljc"]}]})

