(ns aoc2021.day11
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

(def size 10)
(def input (parse-input (aoc/read-file "in11")))

(defn adj-locs [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [nx (+ x dx)
              ny (+ y dy)]
        :when (and (>= nx 0) (>= ny 0)
                 (< nx size)(< ny size)
                 (not= 0 dx dy))]
    [nx ny]))

(defn inc-grid [state]
  (mapv #(mapv inc %) state))

(defn flash [state]
  (loop [flashing-locs #{}
         s state]
    (let [new-locs (for [x (range size)
                         y (range size)
                         :let [v (get-in s [x y])]
                         :when (and (> v 9)
                                  (nil? (flashing-locs [x y])))]
                     [x y])]
      (if (empty? new-locs)
        {:flashes flashing-locs
         :state s}
        (recur (set/union flashing-locs (set new-locs))
               (reduce (fn [ns loc]
                         (update-in ns loc inc))
                       s
                       (mapcat adj-locs new-locs)))))))
(defn deplete [{f :flashes
                s :state}]
  (reduce (fn [ns loc]
            (assoc-in ns loc 0))
          s f))

(defn step [[state c]]
  (let [{flashes :flashes :as step-12} (flash (inc-grid state))]
    [(deplete step-12) (+ c (count flashes))]))

(def steps (iterate step [input 0]))

(second (nth steps 100)) ; 1773

(defn total [[state]]
  (apply + (flatten state)))

(count (take-while #(> (total %) 0) steps)) ; 494

