(ns snake.domain)

(def ^:private world {:food-amount 20})

(defn gen-food [bounds]
  (map rand-int bounds))

(defn replenish-food [food amount bounds]
  (->> (repeatedly #(gen-food bounds))
       (concat food)
       (take amount)
       set))

(defn grow-snake [{:keys [snake velocity] :as state}]
  (let [new-head (->> (peek snake)
                      (map + velocity))
        bounds (:bounds state)
        wrapped-new-head (map mod new-head bounds)]
    (-> state (update :snake conj wrapped-new-head))))

(defn eat [{:keys [snake food] :as state}]
  (if-let [pellet (food (peek snake))]
    (-> state (update :food disj pellet))
    (-> state (update :snake subvec 1))))

(defn create-state [bounds]
  {:bounds   bounds
   :snake    [(map / bounds [2 2])]
   :velocity [0 0]
   :food     #{}})

(defn reset? [{:keys [snake] :as state}]
  (if (apply distinct? snake)
    state
    (create-state (:bounds state))))

(defn update-state [state]
  (-> state
      (update :food replenish-food (:food-amount world) (:bounds state))
      grow-snake
      eat
      reset?))

(defn turn [{:keys [velocity] :as state} dir]
  (if (and dir (not= (map - dir) velocity))
    (assoc state :velocity dir)
    state))

(defn compute-tiles [target-bounds state]
  (let [[sx sy] (map / target-bounds (:bounds state))
        to-tiles (fn [key]
                   (->> (state key)
                        (map #(concat % [1 1]))
                        (map #(map * % [sx sy sx sy]))
                        (map #(hash-map :type key :bounds %1))))]
    (mapcat to-tiles [:snake :food])))


(defn score [{:keys [snake]}]
  (count snake))
