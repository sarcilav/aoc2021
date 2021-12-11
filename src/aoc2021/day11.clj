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

(defn first-substep [lvls]
  (mapv #(mapv inc %) lvls))

(defn second-substep [lvls]
  (loop [flashing-octos #{}
         l lvls]
    (let [new-flashes (for [x (range size)
                            y (range size)
                            :let [v (get-in l [x y])]
                            :when (and (> v 9)
                                     (nil? (flashing-octos [x y])))]
                        [x y])]
      (if (empty? new-flashes)
        {:flashes flashing-octos
         :lvls l}
        (recur (set/union flashing-octos (set new-flashes))
               (reduce (fn [nl pos]
                         (update-in nl pos inc))
                       l
                       (mapcat adj-locs new-flashes)))))))

(defn third-substep [{flashes :flashes
                      lvls :lvls}]
  (reduce (fn [nl pos]
            (assoc-in nl pos 0))
          lvls flashes))

(defn step [[lvls c]]
  (let [{flashes :flashes :as step-12} (second-substep (first-substep lvls))]
    [(third-substep step-12) (+ c (count flashes))]))

(def steps (iterate step [input 0]))

(nth steps 100) ; [[[6 2 2 7 5 5 6 3 4 1] [2 2 2 9 6 5 6 9 6 6] [2 2 2 6 8 6 6 9 3 0] [2 2 2 7 5 9 9 1 3 0] [2 2 8 5 4 5 1 1 2 2] [2 2 7 4 4 5 8 1 1 1] [2 2 8 5 4 5 8 5 1 1] [3 7 5 8 7 8 4 4 5 1] [0 8 7 0 0 0 0 5 4 6] [0 0 0 0 0 0 0 0 6 4]] 1773]

(defn total [[lvls]]
  (apply + (flatten lvls)))

(count (take-while #(> (total %) 0) steps)) ; 494

