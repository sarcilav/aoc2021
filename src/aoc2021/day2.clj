(ns aoc2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (parse-input (slurp (io/resource "in2"))))

(defn parse-input [input]
  (->> input
     (str/split-lines)
     (map #(str/split % #" "))
     (map (fn [[cmd value]]
            [(keyword cmd) (Long/parseLong value)]))))

(defn eval [[hpos d] [cmd v]]
  (case cmd
    :forward [(+ hpos v) d]
    :up      [hpos (- d v)]
    :down    [hpos (+ d v)]))

(defn part1 [input]
  (->> input
     (reduce eval [0 0])
     (apply *)))

(defn eval-2 [[hpos d aim] [cmd v]]
  (case cmd
    :forward [(+ hpos v) (+ d (* aim v)) aim]
    :up      [hpos d (- aim v)]
    :down    [hpos d (+ aim v)]))

(defn part2 [numbers]
  (->> input
     (reduce eval-2 [0 0 0])
     (take 2)
     (apply *))
)

(part1 input) ; 2039256
(part2 input) ; 1856459736

