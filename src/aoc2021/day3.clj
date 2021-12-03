(ns aoc2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (parse-input (slurp (io/resource "in3"))))

(defn parse-input [input]
  (->> input
     (str/split-lines)
     (map char-array)))


(defn part1 [in size]
  (->> in
     (apply map vector)
     (map #(filter #{\1} %))
     (map count)
     (map #(if (> % (/ size 2))
             [\1 \0]
             [\0 \1]))
     (apply map vector)
     (map #(apply str %))
     (map #(Integer/parseInt % 2))
     (apply *)))


(defn fn-common-bit-at [f entries pos]
  (->> entries
     (map #(nth % pos))
     (frequencies)
     (f)))

(defn filter-rating [entries filtering]
  (-> (reduce (fn [acc i]
               (let [bit (filtering acc i)
                     ret (filter #(= (nth % i) bit) acc)]
                 (if (== (count ret) 1)
                   (reduced ret)
                   ret)))
             entries
             (range (count (first entries))))
     (first)))

(defn bits-array-to-int [bits]
  (Integer/parseInt (apply str bits) 2))

(defn max-fn [vals]
  ((comp first last) (sort-by (juxt second first) vals)))

(defn min-fn [vals]
  (ffirst (sort-by (juxt second first) vals)))

(defn part2 [in]
  (let [o2-rating (filter-rating in (partial fn-common-bit-at max-fn))
        co2-rating (filter-rating in (partial fn-common-bit-at min-fn))]
    (* (bits-array-to-int o2-rating)
       (bits-array-to-int co2-rating))) 
  )

(part1 input (count input)) ; 4160394 
(part2 input) ; 4125600


