(ns advent.day6
  (:require [clojure.string :as str]))

(def x ["eedadn"
        "drvtee"
        "eandsr"
        "raavrd"
        "atevrs"
        "tsrnev"
        "sdttsa"
        "rasrtv"
        "nssdts"
        "ntnada"
        "svetve"
        "tesnvt"
        "vntsnd"
        "vrdear"
        "dvrsen"
        "enarar"])

(def input
  (str/split-lines (slurp "input/input6.txt")))

(defn column
  [rows i]
  (map #(nth % i) rows))

(defn ecc1
  [s]
  (->> s
       frequencies
       (sort-by second)
       reverse
       ffirst))

(defn ecc2
  [s]
  (->> s
       frequencies
       (sort-by second)
       ffirst))

(defn decode
  [ecc input]
  (->> (range (count (first input)))
       (map (comp ecc #(column input %)))
       (str/join)))

(def part1 (partial decode ecc1))

(def part2 (partial decode ecc2))

(part1 x)
(part2 x)

(part1 input)
(part2 input)
