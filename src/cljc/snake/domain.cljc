(ns snake.domain
  (:require [clojure.spec.alpha :as s]))

(def foods [:food-1 :food-2 :food-3])
(def snake-parts [:zuma :snake :skye :marshall])

(defn- gen-food [bounds]
  {:position (map rand-int bounds)
   :type     (rand-nth foods)})

(defn- replenish-food [food amount bounds]
  (->> (repeatedly #(gen-food bounds))
       (concat food)
       (take amount)
       set))


(defn- grow-snake [{:keys [::snake ::velocity ::bounds] :as state}]
  (let [new-head         (->> (peek snake)
                              (map + velocity))
        wrapped-new-head (map mod new-head bounds)]
    (-> state (update ::snake conj wrapped-new-head))))

(defn- eat [{:keys [::snake ::food] :as state}]
  (if-let [pellet (some #{(peek snake)} (map :position food))]
    (-> state
        (update ::food (fn [food] (remove #(= (:position %) pellet) food)))
        (update ::events conj :food-consumed))
    (-> state
        (update ::snake subvec 1))))

(s/def ::state
  (s/keys :req [::bounds
                ::clock
                ::clock-rate
                ::snake
                ::actions
                ::events
                ::velocity
                ::food
                ::food-amount]))

(s/fdef create-state :args (s/cat :v vector?) :ret ::state)
(defn create-state [bounds]
  {::bounds      bounds
   ::clock       0
   ::clock-rate  10
   ::snake       [(map / bounds [2 2])]
   ::actions     []
   ::events      []
   ::velocity    [0 0]
   ::food        []
   ::food-amount (/ (apply * bounds) 30)})

(defn- reset? [{:keys [::snake] :as state}]
  (if (apply distinct? snake)
    state
    (-> (create-state (::bounds state))
        (update ::events conj :game-ended))))

(defn action [{:keys [::actions ::velocity] :as state}]
  (if-let [[_ dir] (first actions)]
    (if (and dir (not= (map - dir) velocity))
      (-> state
          (update ::actions empty)
          (assoc ::velocity dir))
      (-> state
          (update ::actions rest)))
    state))

(s/fdef update-state :args (s/cat :s ::state) :ret ::state)

(defn update-state [state]
  (let [updated (-> state
                    (update ::clock inc)
                    (update ::events empty))]
    (if (= 0 (mod (::clock state) (::clock-rate state)))
      (-> updated
          (action)
          (update ::food replenish-food
                  (::food-amount updated)
                  (::bounds updated))
          grow-snake
          eat
          reset?)
      updated)))

(defn turn [state dir]
  (update state ::actions conj [:turn dir]))

(defn drawable-items [state]
  (if state
    (let [[sh & st] snake-parts
          snake-types  (concat [sh] (cycle st))
          snake-points (rseq (::snake state))
          snake-items  (map #(hash-map :type %1 :position %2) snake-types snake-points)
          items        (concat snake-items (::food state))]
      items)
    #{}))

(defn compute-tiles
  ([drawable-items source-bounds target-bounds]
   (if source-bounds
     (let [[sx sy]      (map / target-bounds source-bounds)
           scale        [sx sy sx sy]
           item-to-box  (fn [{position :position :as item}]
                          (-> item
                              (assoc  :bounds  (concat position [1 1]))
                              (dissoc :position)))
           boxes        (map item-to-box drawable-items)
           scalef       (fn [item]
                          (update item :bounds #(map * % scale)))]

       (->> (map  scalef boxes)
            (into #{})))
     #{}))
  ([state target-bounds]
   (let [source-bounds  (::bounds state)
         drawable-items (drawable-items state)]
     (compute-tiles drawable-items source-bounds target-bounds))))

(comment
  (assert
   (=
    #{{:bounds '(4 4 4 4)} {:bounds '(0 0 4 4)}}
    (compute-tiles [{:position [0 0]}
                    {:position [1 1]}] [3 3] [12 12])))
  (sort-by :type (compute-tiles [[0 0]] [3 3] [11 11])))

(defn score [{:keys [::snake]}]
  (dec (count snake)))
