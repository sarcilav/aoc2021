(ns aoc2021.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn make-segment [s]
  (->> s
     (re-seq #"\d+")
     (mapv #(Long/parseLong %))))

(defn parse-input [input]
  (->> input
     (str/split-lines)
     (mapv make-segment)))

(def input (parse-input (slurp (io/resource "in5"))))

(defn direction [x]
  (if (zero? x)
    0
    (/ x (Math/abs x))))

(defn lattice [[a1 b1 a2 b2]]
  (let [delta [(direction (- a2 a1)) (direction (- b2 b1))]]
    (loop [curr [a1 b1]
           points [curr]]
      (if (= curr [a2 b2])
        points
        (let [new (mapv + curr delta)]
          (recur new (conj points new)))))))

(defn solve [segments]
  (->> segments
     (mapcat lattice)
     (frequencies)
     (vals)
     (filter #(> % 1))
     (count)))

(defn parallel-to-axis? [[a1 b1 a2 b2]]
  (or (== a1 a2) (== b1 b2)))

(defn part1 [in]
  (->> in
     (filter parallel-to-axis?)
     (solve)))

(defn part2 [in]
  (->> in
     (solve)))

(part1 input) ; 6225
(part2 input) ; 22116
