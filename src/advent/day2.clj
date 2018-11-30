(ns advent.day2
  (:require [clojure.string :as str]))

(def dir
  {\U [0 1]
   \L [-1 0]
   \R [1 0]
   \D [0 -1]})

(def start1 {:pos [0 0] :buttons []})

(def start2 {:pos [-2 0] :buttons []})

(def keypad1
  ["....."
   ".123."
   ".456."
   ".789."
   "....."])

(def keypad2
  ["......."
   "...1..."
   "..234.."
   ".56789."
   "..ABC.."
   "...D..."
   "......."])

@(def input
   (str/split (slurp "input/input2.txt") #"\n"))

(defn digit [keypad [x y]]
  (let [n (count keypad)]
    (-> keypad
        (nth (- (/ n 2) y))
        (nth (+ x (/ n 2))))))

;;(digit keypad1 [-1 0])

(defn valid-pos?
  [keypad pos]
  (not= \. (digit keypad pos)))

(defn f
  [keypad pos x]
  (let [pos2 (map + pos (dir x))]
    (if (valid-pos? keypad pos2)
      pos2
      pos)))

(defn g
  [keypad {:keys [pos buttons] :as state} code]
  (let [pos2 (reduce (partial f keypad) pos code)]
    (-> state
        (assoc :pos pos2)
        (update :buttons conj (digit keypad pos2)))))

(defn part1
  [input]
  (->> (reduce (partial g keypad1) start1 input)
      :buttons
      (apply str)))

(defn part2
  [input]
  (->> (reduce (partial g keypad2) start2 input)
       :buttons
       (apply str)))

(part1 ["ULL" "RRDDD" "LURDL" "UUUUD"])
(part1 input)

(part2 ["ULL" "RRDDD" "LURDL" "UUUUD"])
(part2 input)
