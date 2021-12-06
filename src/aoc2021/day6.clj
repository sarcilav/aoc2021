(ns aoc2021.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]))

(defn parse-input [input]
  (->> input
     (aoc/string-to-lvector)))

(def input (parse-input (aoc/read-file "in6")))

(defn new-gen [l]
  {0 (get l 1 0)
   1 (get l 2 0)
   2 (get l 3 0)
   3 (get l 4 0)
   4 (get l 5 0)
   5 (get l 6 0)
   6 (+ (get l 7 0) (get l 0 0))
   7 (get l 8 0)
   8 (get l 0 0)})

(defn gen-latern-fish [fish g]
  (->> fish
     (frequencies)
     (iterate new-gen)
     (take (inc g))
     (last)
     (vals)))

(defn part1 [fish]
  (apply + (gen-latern-fish fish 80)))

(defn part2 [fish]
  (apply + (gen-latern-fish fish 256)))

(part1 input) ; 356190

(part2 input) ; 1617359101538
