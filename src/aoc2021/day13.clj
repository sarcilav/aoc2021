(ns aoc2021.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(defn parse-long [s]
  (Long/parseLong s))

(defn make-dots [lines]
  (for [[_ x y] (re-seq #"(\d+),(\d+)" lines)]
    [(parse-long x) (parse-long y)]))

(defn make-instructions [lines]
  (let [axis->idx {"x" 0 "y" 1}]
    (for [[_ axis n]  (re-seq #"(x|y)=(\d+)" lines)]
      [(axis->idx axis) (parse-long n)])))

(defn parse-input [in]
  (let [[dots instructions] (str/split in #"\n\n")]
    [(set (make-dots dots))
     (make-instructions instructions)]))

(def input (parse-input (aoc/read-file "in13")))

(defn fold-one [value n]
  (- n (Math/abs (- n value))))

(defn fold [dots [axis line]]
  (set (map #(update % axis fold-one line) dots)))

;; part 1
(let [[dots [first-instruction]] input]
  (count (fold dots first-instruction))) ; 814

(defn print-points [points]
  (let [w (apply max (map first points))
        h (apply max (map second points))
        banner  (vec (repeat (inc h) (vec (repeat (inc w) \.))))]
    (->> points
       (reduce (fn [g [x y]] (assoc-in g [y x] \#))
               banner)
       (map println))))
;; part 2
(let [[dots instructions] input]
  (->> instructions
     (reduce fold dots)
     (print-points))) ; PZEHRAER

