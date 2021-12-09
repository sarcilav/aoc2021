(ns aoc2021.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]
            [clojure.set :as set]))

(defn parse-line [line]
  (mapv #(Character/digit % 10) line))

(defn parse-input [in]
  (->> in
     (str/split-lines)
     (mapv parse-line)))

(def input (parse-input (aoc/read-file "in9")))

(defn low-point? [lava-tubes x y]
  (let [max-x (count lava-tubes)
        max-y (count (first lava-tubes))
        adj-locs (for [[dx dy] [[0 -1] [0 1] [-1 0] [1 0]]
                       :let [nx (+ x dx)
                             ny (+ y dy)]
                       :when (and (>= nx 0) (>= ny 0)
                                (< nx max-x)(< ny max-y))]
                   [nx ny])]
    (every? #(< (get-in lava-tubes [x y]) %) (map #(get-in lava-tubes %) adj-locs))))

(defn low-points [lava-tubes]
  (for [x (range (count lava-tubes))
        y (range (count (first lava-tubes)))
        :when (low-point? lava-tubes x y)]
    [x y]))

(defn part1 [in]
  (->> in
     (low-points)
     (map #(get-in in %))
     (map inc)
     (apply +)))

(part1 input) ; 475

(defn basin [lava-tubes [x y] b]
  (let [max-x (count lava-tubes)
        max-y (count (first lava-tubes))
        new-members (set (for [[dx dy] [[0 -1] [0 1] [-1 0] [1 0]]
                               :let [nx (+ x dx)
                                     ny (+ y dy)]
                               :when (and (>= nx 0) (>= ny 0)
                                        (< nx max-x)(< ny max-y))
                               :when (< (get-in lava-tubes [nx ny]) 9)
                               :when (nil? (b [nx ny]))]
                           [nx ny]))]
    (if (nil? new-members)
      b
      (reduce (fn [members new-member]
                (basin lava-tubes new-member (conj members new-member)))
              b
              new-members))))

(defn part2 [in]
  (->> in
     (low-points)
     (map #(basin in % #{%}))
     (map count)
     (sort >)
     (take 3)
     (apply *)))

(part2 input) ; 1092012
