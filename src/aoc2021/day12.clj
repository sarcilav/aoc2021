(ns aoc2021.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2021.core :as aoc]
            [clojure.set :as set]))

(defn parse-input [in]
  (->> in
     (str/split-lines)
     (map #(str/split % #"-"))))

(def input (parse-input (aoc/read-file "in12")))

(defn make-graph [in]
  (let [link (fn [edges start end]
               (reduce-kv (fn [h k vs]
                            (assoc h k (map end vs)))
                          {}
                          (group-by start edges)))]
   (merge-with into
               (link in first second)
               (link in second first))))

(defn big-cave? [cave]
  (= (str/upper-case cave) cave))

(defn traverse [g path f]
  (let [curr (first path)
        visited (remove big-cave? path)
        visited (if (f visited) visited #{"start"}) ;; part2 injection
        visitable (remove (set visited) (g curr))]
    (if (= curr "end")
      '(path)
      (mapcat #(traverse g (conj path %) f) visitable))))

(def graph (make-graph input))

(count (traverse graph '("start") seq)) ; 3230
(time (count (traverse graph '("start")
                  (fn [v] (some #{2} (-> (frequencies v)
                                       (vals))))))) ; 83475
