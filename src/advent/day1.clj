(ns advent.day1
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(def x1 [[:R 2] [:L 3]])
(def x2 [[:R 2] [:R 2] [:R 2]])
(def x3 [[:R 5] [:L 5] [:R 5] [:R 3]])
(def x4 [[:R 8] [:R 4] [:R 4] [:R 8]])

(def start {:pos [0 0] :face [0 1] :visits #{}})

(defn parse-instruction
  [s]
  [(keyword (subs s 0 1))
   (read-string (subs s 1))])

(defn expand
  [[d n]]
  (concat
    [[d 1]]
    (repeat (dec n) [:* 1])))

(def expand* (partial mapcat expand))

@(def input
  (->>
    (str/split (slurp "input/input1.txt") #", ")
    (map parse-instruction)
    expand*))

(defn turn
  [[x y] d]
  (case d
    :R [y (- x)]
    :L [(- y) x]
    :* [x y]))

(defn step
  [[x y] [dx dy] n]
  [(+ x (* n dx))
   (+ y (* n dy))])

(defn f
  [{:keys [pos face visits twice]} [d n]]
  (let [face (turn face d)
        pos (step pos face n)]
    {:pos pos
     :face face
     :twice (or twice (visits pos))
     :visits (conj visits pos)}))

(defn part1 [input]
  (->> (reduce f start input)
       :pos
       (map #(Math/abs %))
       (apply +)))

(defn part2 [input]
  (->> (reduce f start (expand* input))
       :twice
       (map #(Math/abs %))
       (apply +)))

(part1 input)
(part2 input)
