(ns snake.domain-test
  (:require [clojure.test :refer [deftest is] :as t]
            [clojure.spec.test.alpha :as stest]
            [snake.domain :as d
             :refer [create-state
                     update-state
                     turn
                     score
                     compute-tiles]]))

(stest/instrument `update-state)
(stest/instrument `create-state)



(comment
  (remove-ns 'snake.domain-test)
  (t/run-tests)
  )

(deftest apply-actions
  (t/are [act0 v0 _ act1 v1]
         (= {::d/actions act1 ::d/velocity v1}
            (d/action {::d/actions act0 ::d/velocity v0}))
    [] [] :then [] []
    [] [0 0] :then [] [0 0]
    [[:move [1 0]]] [1 0] :then [] [1 0]
    [[:move [1 0]]] [0 0] :then [] [1 0]
    [[:move [1 0]] [:move [1 0]]] [1 0] :then [] [1 0]
    [[:move [-1 0]] [:move [1 0]]] [1 0] :then [] [1 0]
    [[:move [0 1]]] [1 0] :then [] [0 1])

  )

(comment
 (deftest eat-stuff
  (let [{:keys [::d/snake 
                ::d/events 
                ::d/food] :as s}
        (with-redefs [rand-int (fn [n] (int (/ n 3)))]
          (-> (create-state [10 10])
              (assoc ::d/clock-rate 1)
              (turn [-1 0])
              (update-state)
              (update-state)
              (turn [0 -1])
              (update-state)
              (update-state)))]
    (is (= 1 (score s)))
    (is (empty? food))
    (is (= snake [[3 4] [3 3]]))
    (is (= events [:food-consumed])))))

(deftest tiles
  (is (= [{:type :food-1, :bounds [6 6 1 1]}
          {:type :food-2, :bounds [7 7 1 1]}
          {:type :green-head,   :bounds [8 9 1 1]}]
         (sort-by :type
                  (compute-tiles
                   {::d/bounds [20 20]
                    ::d/snake  [[8 9]]
                    ::d/food   [{:position [6 6]
                                 :type :food-1}
                                {:position [7 7]
                                 :type :food-2}]}
                   [20 20])))))

(comment
  (d/drawable-items
   {::d/bounds [20 20]
    ::d/snake  [[8 9]]
    ::d/food [{:position [6 6] :type :food-1}
              {:position [7 7] :type :food-2}]}))