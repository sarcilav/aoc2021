(ns aoc2021.core
  (:require [clojure.java.io :as io]))

(defn parse-long [s]
  (Long/parseLong s))

(defn string-to-lvector [s]
  (->> s
     (re-seq #"\d+")
     (mapv parse-long)))

(defn read-file [name]
  (slurp (io/resource name)))
