(ns advent.day3
  (:require [clojure.string :as str]))

(def raw (slurp "input/input3.txt"))

(def input
  (->>
    (str/split raw #"\n")
    (map str/trim)
    (map (comp (partial map read-string)
               #(str/split % #"\s+")))))

(def input2
  (->> input
       ((juxt
          (partial map first)
          (partial map second)
          (partial map last)))
       flatten
       (partition 3)))

(defn valid?
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(defn count-triangles
  [input]
  (->> input
       (filter valid?)
       count))

(count-triangles input)
(count-triangles input2)
