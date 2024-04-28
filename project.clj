(defproject snake-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cloverage "1.2.2"]]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [quil "4.3.1323"]
                 [org.craigandera/dynne "0.4.1"]]
  :main snake.launcher
  :source-paths ["src/clj" "src/cljc"])
