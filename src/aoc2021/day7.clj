(ns aoc2021.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]))

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (mapv aoc/parse-long)))

(def input (parse-input (aoc/read-file "in7")))

(defn cost-fn [fuel-cost pos->n apos]
  (reduce-kv (fn [acc pos n]
               (+ acc (* (fuel-cost (Math/abs (- apos pos))) n)))
             0
             pos->n))

(defn solve [cost positions]
  (loop [[low-p high-p] (apply (juxt min max) positions)
         low-v (cost low-p)
         high-v (cost high-p)]
    (if (= (inc low-p) high-p)
      (min low-v high-v)
      (let [mid-p (quot (+ low-p high-p) 2)
            mid-v (cost mid-p)]
        (if (< low-v high-v)
          (recur [low-p mid-p] low-v mid-v)
          (recur [mid-p high-p] mid-v high-v))))))

(def freqs (frequencies input))

(defn part1 [input]
  (solve (partial cost-fn identity freqs) input))

(part1 input) ; 353800

(defn fuel [n]
  (/ (* n (inc n)) 2))

(defn part2 [input]
  (solve (partial cost-fn fuel freqs) input))

(part2 input) ; 98119739
