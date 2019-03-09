(ns snake.utils)

(defn as-map [f keys]
  (->> (map (juxt identity f) keys)
       (into {})))
