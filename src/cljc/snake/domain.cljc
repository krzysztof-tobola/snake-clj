(ns snake.domain)

(defn- gen-food [bounds]
  {:position (map rand-int bounds)})

(defn- replenish-food [food amount bounds]
  (->> (repeatedly #(gen-food bounds))
       (concat food)
       (take amount)
       set))

(defn- grow-snake [{:keys [snake velocity bounds] :as state}]
  (let [new-head         (->> (peek snake)
                              (map + velocity))
        wrapped-new-head (map mod new-head bounds)]
    (-> state (update :snake conj wrapped-new-head))))

(defn- eat [{:keys [snake food] :as state}]
  (if-let [pellet (some #{{:position (peek snake)}} food)]
    (-> state
        (update :food disj pellet)
        (update :events conj :food-consumed))
    (-> state
        (update :snake subvec 1))))

(defn create-state [bounds]
  {:bounds      bounds
   :snake       [(map / bounds [2 2])]
   :events      []
   :velocity    [0 0]
   :food        #{}
   :food-amount (/ (apply * bounds) 10)})

(defn- reset? [{:keys [snake] :as state}]
  (if (apply distinct? snake)
    state
    (create-state (:bounds state))))

(defn update-state [state]
  (-> state
      (update :events empty)
      (update :food replenish-food (:food-amount state) (:bounds state))
      grow-snake
      eat
      reset?))

(defn turn [{:keys [velocity] :as state} dir]
  (if (and dir (not= (map - dir) velocity))
    (assoc state :velocity dir)
    state))

(defn compute-tiles [target-bounds state]
  (let [[sx sy] (map / target-bounds (:bounds state))
        to-tiles (fn [type points]
                   (->> points
                        (map #(concat % [1 1]))
                        (map #(map * % [sx sy sx sy]))
                        (map #(hash-map :type type :bounds %1))))
        snake    (rseq (:snake state))]
    (concat
      (mapcat #(to-tiles %1 (vector %2)) (cycle [:zuma :snake :skye :marshall]) snake)
      ;todo food should be a seq instead of set
      (mapcat #(to-tiles %1 (vector %2))
              (cycle [:food-1 :food-2])
              (reverse (map :position (:food state)))))))

(defn score [{:keys [snake]}]
  (count snake))

