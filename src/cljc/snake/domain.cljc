(ns snake.domain)

(def foods [:food-1 :food-2 :food-3])
(def snake-parts [:zuma :snake :skye :marshall])
(def snake-parts [:green-head :green-tail])

(defn- gen-food [bounds]
  {:position (map rand-int bounds)
   :type     (rand-nth foods)})

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
  (if-let [pellet (some #{(peek snake)} (map :position food))]
    (-> state
        (update :food (fn [food] (remove #(= (:position %) pellet) food)))
        (update :events conj :food-consumed))
    (-> state
        (update :snake subvec 1))))

(defn create-state [bounds]
  {:bounds      bounds
   :clock       0
   :clock-rate  10
   :snake       [(map / bounds [2 2])]
   :actions     []
   :events      []
   :velocity    [0 0]
   :food        []
   :food-amount (/ (apply * bounds) 30)})

(defn- reset? [{:keys [snake] :as state}]
  (if (apply distinct? snake)
    state
    (-> (create-state (:bounds state))
        (update :events conj :game-ended))))

(defn action [{:keys [actions velocity] :as state}]
  (if-let [[_ dir] (first actions)]
    (if (and dir (not= (map - dir) velocity))
      (-> state
          (update :actions empty)
          (assoc :velocity dir))
      (-> state
          (update :actions rest)))
    state))

(defn update-state [state]
  (let [updated (-> state
                    (update :clock inc)
                    (update :events empty))]
    (if (= 0 (mod (:clock state) (:clock-rate state)))
      (-> updated
          (action)
          (update :food replenish-food (:food-amount updated) (:bounds updated))
          grow-snake
          eat
          reset?)
      updated)))

(defn turn [state dir]
  (update state :actions conj [:turn dir]))

(defn drawable-items [state]
  (if state
    (let [[sh & st] snake-parts
          snake-types  (concat [sh] (cycle st))
          snake-points (rseq (:snake state))
          snake-items  (map #(hash-map :type %1 :position %2) snake-types snake-points)
          items        (concat snake-items (:food state))]
      items)
    #{}))

(defn compute-tiles [state target-bounds]
  (let [source-bounds  (:bounds state)
        drawable-items (drawable-items state)]
    (if source-bounds
      (let [[sx sy] (map / target-bounds source-bounds)
            scale    [sx sy sx sy]
            to-tiles (fn [{position :position :as item}]
                       (assoc item :bounds (map * (concat position [1 1]) scale)))]

        (->> (map to-tiles drawable-items)
             (into #{})))
      #{})))

(comment
  (drawable-items tmp)
  (sort-by :type (compute-tiles tmp [24 14])))

(defn score [{:keys [snake]}]
  (dec (count snake)))
