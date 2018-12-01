(ns advent.day5
  (:require [digest]
            [clojure.string :as str]))

(def input "uqwqemis")

(defn xhash
  [salt n]
  (digest/md5
    (str salt n)))

(defn generator
  [input]
  (->> (iterate inc 0)
       (map (partial xhash input))
       (filter #(str/starts-with? % "00000"))
       (map (partial drop 5))))

(defn part1
  [input]
  (->> (generator input)
       (map first)
       (take 8)
       (str/join)))

(defn rf
  [{:keys [seen pairs] :as state} c]
  (cond
    (seen (first c)) state
    (>= (int (first c)) (int \8)) state
    :else (-> state
              (update :seen conj (first c))
              (update :pairs conj (take 2 c)))))

(defn rf
  [m c]
  (cond
    (m (first c)) m
    (>= (int (first c)) (int \8)) m
    :else (assoc m (first c) (take 2 c))))

(defn part2
  [input]
  (->> (generator input)
       (reductions rf {})
       (drop-while #(< (count %) 8))
       (first)
       (vals)
       (sort-by first)
       (map second)
       (str/join)))

;;(part1 input)
;;(part2 input)
