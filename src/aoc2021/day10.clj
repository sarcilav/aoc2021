(ns aoc2021.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]
            [clojure.set :as set]))

(defn parse-input [in]
  (->> in
     (str/split-lines)))

(def input (parse-input (aoc/read-file "in10")))

(def pairs  {
             \< \>
             \( \)
             \[ \]
             \{ \}
             })

(defn balance? [s]
  (reduce (fn [stack x]
            (if (pairs x)
              (conj stack x)
              (if (= (pairs (peek stack)) x)
                (pop stack)
                (reduced {:corrupted x}))))
          [] s))

(defn checker [s]
  (let [checked-string (balance? s)]
    (if (vector? checked-string)
      {:incomplete checked-string}
      checked-string)))

(defn part1 [in]
  (->> in
     (map checker)
     (map :corrupted)
     (remove nil?)
     (map {\) 3 \] 57 \} 1197 \> 25137})
     (apply +)))

(part1 input) ; 316851

(defn complement [s]
  (mapv pairs (rseq s)))

(defn cost [s]
  (->> s
     complement
     (map {\) 1 \] 2 \} 3 \> 4})
     (reduce #(+ (* %1 5) %2) 0)))

(defn middle [l]
  (let [middle (quot (count l) 2)]
    (nth l middle)))

(defn part2 [in]
  (->> in
     (map checker)
     (map :incomplete)
     (remove nil?)
     (map cost)
     sort
     middle))

(part2 input) ; 2182912364 
