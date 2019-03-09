(ns snake.gui
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [snake.domain :as s]
            [snake.utils :refer :all]))

(def frame-rate 10)
(def key-to-direction {37 [-1 0]
                       39 [1 0]
                       38 [0 -1]
                       40 [0 1]})

(defn load-image [symbol]
  (->> (name symbol)
       (format "resources/%s.png")
       (q/load-image)))

(defn setup []
  (do (q/smooth)
      {:game-state (s/create-state)
       :images     (as-map load-image [:food :snake])}))

(defn key-pressed [state {:keys [key-code]}]
  (update state :game-state s/turn (key-to-direction key-code)))

(defn draw [{:keys [game-state images]}]
  (let [[w h :as dims] [(q/width) (q/height)]
        tiles (s/compute-tiles dims game-state)
        score (str "Score: " (s/score game-state))]
    (do
      (q/frame-rate frame-rate)
      (q/background 30 40 25)

      (doseq
        [{[x y w h] :bounds tile-type :type} tiles]
        (q/image (images tile-type) x y w h))

      (q/fill 250 100 100)
      (q/text-size (/ h 30))
      (q/text score (/ w 60) (/ h 30)))))

(defn launch-sketch []
  (q/sketch
    :title "Snake"
    :setup setup
    :update #(update % :game-state s/update-state)
    :draw #(draw %)
    :key-pressed #(key-pressed %1 %2)
    :middleware [m/fun-mode]
    :size [(- (q/screen-width) 100) (- (q/screen-height) 100)]))

(comment
  (def sketch (launch-sketch))
  (.exit sketch))
