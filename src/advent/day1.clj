(ns advent.day1
  (:require [clojure.string :as str]))

(def steps
  (-> "input/input1.txt"
      slurp
      (str/split #",")
      (#(map str/trim %))))

(defn turn [[x y] d]
  (case d
    \R [y (- x)]
    \L [(- y) x]))

(defn move [[x y] [dx dy] n]
  [(+ x (* n dx))
   (+ y (* n dy))])

(defn walk [[pos face] step]
  (let [n (Integer/parseInt (subs step 1))
        d (get step 0)]
    [(move pos (turn face d) n)
     (turn face d)]))

(defn solve [steps]
  (loop [state [[0 0] [0 1]]
         steps steps]
    (if-let [step (first steps)]
      (recur (walk state step) (rest steps))
      (reduce + (map #(Math/abs %) (first state))))))

(solve steps)
