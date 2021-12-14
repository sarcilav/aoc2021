(ns aoc2021.day14
  (:require [clojure.java.io :as io]
            [aoc2021.core :as aoc]))
        
(defn parse-input [input]
  (let [[template rest] (str/split input #"\n\n")
        reactions  (->> rest
                  (re-seq #"(.*) -> (.*)")
                  (map (fn [[_ [a b] [c]]] [[a b] c]))
                  (into {}))]
    [template reactions]))

(def input (parse-input (aoc/read-file "in14")))

(defn grow [b-freqs reactions]
  (reduce-kv (fn [acc [a b] count]
               (if-let [c (reactions [a b])]
                 (-> acc
                     (update [a c] (fnil + 0) count)
                     (update [c b] (fnil + 0) count))
                 acc))
             {}
             b-freqs))

(defn chain [b-mers-freqs reactions]
  (iterate #(grow % reactions) b-mers-freqs))

(defn solve [in steps]
  (let [[base-polymer reactions] in
        binomers-freqs (frequencies (partition 2 1 base-polymer))]
    (->> (nth (chain binomers-freqs reactions) steps)
       (reduce-kv (fn [m [a] v] (update m a (fnil + 0) v))
                  {(last base-polymer) 1})
       vals
       (apply (juxt max min))
       (apply -))))

;; part 1
(time (solve input 10)) ; 3048

;; part 2
(time (solve input 40)) ; 3288891573057

