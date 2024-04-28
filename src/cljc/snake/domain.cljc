(ns snake.domain
  (:require [clojure.spec.alpha :as s]))

(def foods [:food-1 :food-2 :food-3])
(def snake-parts [:green-head :green-tail])

(defn- gen-food [bounds]
  {:position (map rand-int bounds)
   :type     (rand-nth foods)})

(defn- replenish-food [{:keys [::food ::food-amount ::bounds] :as state}]
  (let [new-food (->> (repeatedly #(gen-food bounds))
                      (concat food)
                      (distinct)
                      (take food-amount)
                      (into #{}))]
    (assoc state ::food new-food)))


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

(s/def ::bounds (s/cat :w int? :h int?))
(s/def ::time-millis int?)
(s/def ::clock int?)
(s/def ::clock-rate int?)
(s/def ::point (s/cat :x int? :y int?))
(s/def ::velocity ::point)
(s/def ::snake (s/* (s/spec ::point)))
(s/def ::item (s/keys :req-un [::position ::type]))
(s/def ::food (s/coll-of ::item))

(s/def ::state
  (s/keys :req [::bounds
                ::time-millis
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
   ::time-millis 0
   ::clock       0
   ::clock-rate  10
   ::snake       [(map / bounds [2 2])]
   ::actions     []
   ::events      []
   ::velocity    [0 0]
   ::food        []
   ::food-amount 1})

(defn reset [{:keys [::bounds]}]
  (-> (create-state bounds)
      (update ::events conj :game-ended)))

(defn- reset? [{:keys [::snake] :as state}]
  (if (apply distinct? snake)
    state
    (reset state)))

(defn drop-until [pred xs]
  (drop-while (complement pred) xs))

(defn action [{:keys [::actions ::velocity] :as state}]
  (let [valid? (fn [[type dir]] (and
                                 dir
                                 (not= (map - dir) velocity)
                                 (not= dir velocity)))
        valid-actions (drop-until valid? actions)]
    (if (empty? valid-actions)
      (assoc state ::actions [])
      (do
        (println "valid actions"  valid-actions)
        (-> state
            (assoc ::velocity (second (first valid-actions)))
            (assoc ::actions (rest valid-actions)))))))

(s/fdef update-state :args (s/cat :input-state ::state) :ret ::state)


;work in progress
(defn timed-update [state time-millis f]
  (let [current-clock (::clock state)
        new-clock (quot time-millis 10)
        diff (- new-clock current-clock)]
    (-> (assoc state ::time-millis time-millis)
        (assoc ::clock new-clock))))

(comment

  (= (timed-update (create-state [10 10]) 9 #(update % ::actions conj :x))
     (-> (create-state [10 10])
         (assoc ::time-millis 9)))

  (= (timed-update (create-state [10 10]) 10 #(update % ::actions conj :x))
     (-> (create-state [10 10])
         (assoc ::time-millis 10)
         (assoc :clock 1)))

  (= (timed-update (create-state [10 10]) 10 #(update % ::actions conj :x))
     (-> (create-state [10 10])
         (assoc ::time-millis 20)
         (assoc :clock 2)))

  ())

(defn update-state
  ([state time-millis]
   (let [updated (-> state
                     (assoc ::time-millis time-millis)
                     (update ::clock inc)
                     (update ::events empty))]
     (if (= 0 (mod (::clock state) (::clock-rate state)))
       (-> updated
           action
           replenish-food
           grow-snake
           eat
           reset?)
       updated)))
  ([state]
   (update-state state 0)))

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
