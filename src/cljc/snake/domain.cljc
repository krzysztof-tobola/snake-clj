(ns snake.domain)

(def ^:private world {:bounds      [30 20]
                      :food-amount 20})

(defn gen-food []
  (map rand-int (:bounds world)))

(defn replenish-food [food amount]
  (->> (repeatedly gen-food)
       (concat food)
       (take amount)
       set))

(defn grow-snake [{:keys [snake velocity] :as state}]
  (let [head (map + (peek snake) velocity)
        bounds (:bounds world)]
    (-> state (update :snake conj (map mod head bounds)))))

(defn eat [{:keys [snake food] :as state}]
  (if-let [pellet (food (peek snake))]
    (-> state (update :food disj pellet))
    (-> state (update :snake subvec 1))))

(defn reset? [{:keys [snake] :as state}]
  (if (apply distinct? snake)
    state
    (assoc state :snake [(peek snake)])))

(defn create-state []
  {:snake    [(map / (:bounds world) [2 2])]
   :velocity [1 0]
   :food     #{}})

(defn update-state [state]
  (-> state
      (update :food replenish-food (:food-amount world))
      grow-snake
      eat
      reset?))

(defn turn [{:keys [velocity] :as state} dir]
  (if (and dir (not= (map - dir) velocity))
    (assoc state :velocity dir)
    state))

(defn compute-tiles [target-bounds state]
  (let [[sx sy] (map / target-bounds (:bounds world))
        to-tiles (fn [key]
                   (->> (state key)
                        (map #(concat % [1 1]))
                        (map #(map * % [sx sy sx sy]))
                        (map #(hash-map :type key :bounds %1))))]
    (mapcat to-tiles [:snake :food])))


(defn score [{:keys [snake]}]
  (count snake))
