(ns aoc2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (parse-input (slurp (io/resource "input_01.txt"))))

(defn parse-input [input]
  (->> input
     (str/split-lines)
     (map #(Long/parseLong %))))

(defn part1 [numbers]
  (->> numbers
     (partition 2 1)
     (filter (fn [[a b]] (<= a b)))
     count))

(defn part2 [numbers]
  (->> numbers
     (partition 3 1)
     (map #(reduce + %))
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count))

(part1 input) ; 1195
(part2 input) ; 1235
