(ns cljc.snake.domain-test
  (:require [clojure.test :refer :all]
            [snake.domain :refer :all]))

(deftest eat-stuff
  (let [{:keys [snake events food] :as s}
        (with-redefs [rand-int (fn [n] (int (/ n 3)))]
          (-> (create-state [10 10])
              (turn [-1 0])
              update-state
              update-state
              (turn [0 -1])
              update-state
              update-state))]
    (is (= 1 (score s)))
    (is (empty? food))
    (is (= snake [[3 4] [3 3]]))
    (is (= events [:food-consumed]))))

(deftest tiles
  (is (= [{:type :food-1, :bounds [6 6 1 1]}
          {:type :food-2, :bounds [7 7 1 1]}
          {:type :zuma, :bounds `(8 9 1 1)}]
         (sort-by :type
                  (compute-tiles {:bounds [20 20],
                                  :snake  [[8 9]],
                                  :food   [{:position [6 6], :type :food-1}
                                           {:position [7 7], :type :food-2}]}
                                 [20 20])))))

(comment)


;(compute-tiles [0 10]))))
