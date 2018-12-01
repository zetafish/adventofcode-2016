(ns advent.day8
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def width 50)
(def height 6)

(insta/defparser p
  "S             = rect | rotate-row | rotate-column
   rect          = <'rect '> num <'x'> num
   rotate-row    = <'rotate row y='> num <' by '> num
   rotate-column = <'rotate column x='> num <' by '> num
   num           = #'[0-9]+'")

(defn parse
  [s]
  (insta/transform
    {:num #(Integer/parseInt %)}
    (second (p s))))

(def input
  (map parse
       (str/split-lines
         (slurp "input/input8.txt"))))

(def start
  (vec (repeat height (vec (repeat width \.)))))

(defn show
  [grid]
  (doseq [x (map str/join grid)]
    (println x))
  (println))

(defmulti update-grid (fn [_ code] (first code)))

(defmethod update-grid :rect
  [grid [_ a b]]
  (reduce (fn [grid [x y]]
            (assoc-in grid [y x] \x))
          grid
          (for [col (range a)
                row (range b)]
            [col row])))

(defmethod update-grid :rotate-row
  [grid [_ y n]]
  (update grid y #(vec (concat (drop (- width n) %)
                               (take (- width n) %)))))

(defmethod update-grid :rotate-column
  [grid [_ x n]]
  (let [col (map #(nth % x) grid)
        col' (concat (drop (- height n) col)
                     (take (- height n) col))]
    (mapv (fn [row v]
            (assoc row x v))
          grid col')))

(defn part1
  [input]
  (->> (reduce update-grid start input)
       flatten
       (filter #(= % \x))
       count))

(defn part2
  [input]
  (->> (reduce update-grid start input)
       show))

(part1 input)
(part2 input)
