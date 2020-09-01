(ns snake.gui
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [snake.domain :as s]
            [snake.utils :refer :all]
            [dynne.sampled-sound :as d]))

(defonce sketches (atom []))
(def frame-rate 60)
(def key-to-direction {37 [-1 0]
                       39 [1 0]
                       38 [0 -1]
                       40 [0 1]})
(def load-image
  (memoize
    (fn [symbol]
      (->> (name symbol)
           (format "resources/%s.png")
           (q/load-image)))))

(def load-sound
  (memoize
    (fn [symbol]
      (->> (name symbol)
           (format "resources/%s.mp3")
           (d/read-sound)))))

(defn setup []
  (do (q/smooth)
      {:game-state (-> (s/create-state (map #(quot % 64) [(q/width) (q/height)])))
       :prev-state nil}))

(defn key-pressed [state {:keys [key-code]}]
  (update state :game-state s/turn (key-to-direction key-code)))

(defn draw [{:keys [game-state prev-state]}]
  (let [[w h :as dims] [(q/width) (q/height)]
        tiles  (s/compute-tiles game-state dims)
        tiles2 (s/compute-tiles prev-state dims)
        score  (str "Score: " (s/score game-state))]
    (when (not= tiles tiles2)
      (do
        (q/frame-rate frame-rate)
        (q/background 30 40 25)
        (doseq [e (:events game-state)]
          (d/play (load-sound e)))
        (doseq
          [{[x y w h] :bounds tile-type :type} tiles]
          (q/image (load-image tile-type) x y w h))

        (q/fill 250 100 100)
        (q/text-size (/ h 30))
        (q/text score (/ w 60) (/ h 30))))))

(defn update-gs [st]
  (-> st
      (assoc :prev-state (:game-state st))
      (update :game-state s/update-state)))

(defn launch-sketch
  ([]
   (launch-sketch (q/screen-width) (- (q/screen-height) 40)))
  ([w h]
   (swap! sketches conj
          (q/sketch
            :title "Snake"
            :setup #'setup
            :update #'update-gs
            :draw #'draw
            :key-pressed #'key-pressed
            :middleware [m/fun-mode]
            :size (map #(full-multiple % 64) [w h])))))

(comment
  (launch-sketch 1536 910)
  (run! #(.exit %) @sketches))
