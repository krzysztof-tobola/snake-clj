(ns cljc.snake.domain-test
  (:require [clojure.test :refer :all]
            [snake.domain :refer :all]))

(deftest eat-stuff
  (is (=
        (with-redefs [rand-int (fn [n] (int (/ n 3)))]
          (-> (create-state [10 10])
              (turn [-1 0])
              update-state
              update-state
              (turn [0 -1])
              update-state
              update-state))
        {:bounds      [10 10],
         :snake       [`(3 4) `(3 3)],
         :events      [:food-consumed],
         :velocity    [0 -1],
         :food        #{},
         :food-amount 10})))