(ns aoc2021.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (->> input
     (str/split-lines)))

(def input (parse-input (slurp (io/resource "in4"))))

(defn line->col [col]
  (map #(Long/parseLong %) (re-seq #"\d+" col)))

(defn order-and-boards [[rand-order-string & boards-str-col]]
  (let [rand-order (line->col rand-order-string)
        boards (->>  boards-str-col
                   (map line->col)
                   (partition 6)
                   (map #(drop 1 %)))]
    [rand-order boards]))

(defn mark [board x]
  (map #(replace {x nil} %) board))

(defn row-check [board]
  (pos? (count (filter #(every? nil? %) board))))

(defn column-check [board]
  (->> board
     (apply map vector)
     (row-check)))

(defn bingo? [board]
  (or (row-check board)
     (column-check board)))

(defn score [curr board]
  (* curr (apply + (filter some? (flatten board)))))

(defn winner-boards-scores [in]
  (let [[order boards] (order-and-boards in)]
    (loop [curr (first order)
           [ftail & tail] (rest order)
           mark-boards (map #(mark % curr) boards)
           scores []]
      (let [{winners true
             alive false} (group-by bingo? mark-boards)]
        (if (some? alive)
          (recur ftail
                 tail
                 (map #(mark % ftail) alive)
                 (vec (concat scores (map #(score curr %) winners))))
          (vec (concat scores (map #(score curr %) winners))))))))

(def scores (winner-boards-scores input))
(first scores) ; 33462
(peek scores) ; 30070
