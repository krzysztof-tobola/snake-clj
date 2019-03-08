(ns snake.launcher
  (:gen-class)
  (:require [snake.gui :as sc]))

(defn -main [& _]
  (sc/launch-sketch))
