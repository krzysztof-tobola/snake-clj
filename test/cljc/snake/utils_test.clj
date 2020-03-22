(ns cljc.snake.utils-test
  (:require [clojure.test :refer :all]
            [snake.utils :refer :all]))

(deftest multiples
  (is (= {0 64, 64 64, 128 64, 192 64}
         (frequencies (map #(full-multiple % 64) (range 256))))))
