(ns aoc2021.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]
            [clojure.set :as set]))

(defn parse-line [l]
  (->> l
     (re-seq #"[abcdefg]+")
     (split-at 10)))

(defn parse-input [input]
  (->> input
     (str/split-lines)
     (map parse-line)))

(def input (parse-input (aoc/read-file "in8")))

(defn part1 [input]
  (->> input
     (map second)
     (flatten)
     (map count)
     (filter #{2 3 4 7})
     (count)))

(part1 input) ; 381

(defn find-bef [signals]
  (->> signals
     (str/join)
     (frequencies)
     (filter #(#{6 4 9} (val %)))
     (map (fn [x] [(get {6 'b
                        4 'e
                        9 'f} (second x))
                  (first x)]))
     (into {})))

(defn find-1478 [signals]
  (->> signals
     (map (fn [x] [(count x) x]))
     (filter #(#{2 3 4 7} (first %)))
     (map (fn [x] [(get {2 1
                        3 7
                        4 4
                        7 8} (first x))
                  (second x)]))
     (into {})))

(defn find-* [letter segments number]
  {letter (first (set/difference (set number) (set (map second segments))))})

(def seven-segment {"abcefg" 0
                    "cf" 1
                    "acdeg" 2
                    "acdfg" 3
                    "bcdf" 4
                    "abdfg" 5
                    "abdefg" 6
                    "acf" 7
                    "abcdefg" 8
                    "abcdfg" 9})

(defn entry->digit [segments entry]
  (seven-segment (str/join (sort (map segments entry)))))

(defn line->number [[signals entry]]
  (let [digits (find-1478 signals)
        segments (find-bef signals)
        segments (conj segments (find-* 'c segments (digits 1)))
        segments (conj segments (find-* 'a segments (digits 7)))
        segments (conj segments (find-* 'd segments (digits 4)))
        segments (conj segments (find-* 'g segments (digits 8)))
        reverse-segments (set/map-invert segments)]
    (->> entry
       (map #(entry->digit reverse-segments %))
       (reduce #(+ (* 10 %1) %2) 0))))

(defn part2 [in]
  (->> in
     (map line->number)
     (reduce +)))

(part2 input) ; 1023686
