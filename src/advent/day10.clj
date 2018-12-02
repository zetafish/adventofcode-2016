(ns advent.day10
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(insta/defparser p
  "<S>     = goes | gives
   goes    = value <' goes to '> bot
   value   = <'value '> num
   gives   = bot <' gives '> low-to <' and '> high-to
   <low-to>  = <'low to '> (bot | output)
   <high-to> = <'high to '> (bot | output)
   bot     = <'bot '> num
   output  = <'output '> num
   num     = #'[0-9]+'")

(defn parse
  [s]
  (first
    (insta/transform
      {:num read-string}
      (p s))))

(def input (->> (slurp "input/input10.txt")
                (str/split-lines)
                (map parse)))

@(def x
  (map parse
   ["value 5 goes to bot 2"
    "bot 2 gives low to bot 1 and high to bot 0"
    "value 3 goes to bot 1"
    "bot 1 gives low to output 1 and high to bot 0"
    "bot 0 gives low to output 2 and high to output 0"
    "value 2 goes to bot 2"]))

(defn initial-state
  [input]
  (->> input
       (filter #(= :goes (first %)))
       (reduce (fn [state [_ [_ value] [_ bot]]]
                 (update-in state [:bot bot] conj value))
               {})))

(defn bot-rules
  [input]
  (->> input
       (filter #(= :gives (first %)))
       (map (fn [[_ [_ bot] low-to high-to]]
              [bot [low-to high-to]]))
       (into {})))

(defn ready-bot
  [state]
  (->> (:bot state)
       (filter #(= 2 (count (second %))))
       ffirst))

(defn remove-val
  [col v]
  (remove #(= % v) col))

(defn step
  [rules state]
  (if-let [bot (ready-bot state)]
    (let [rule (rules bot)
          [low high] (sort (get-in state [:bot bot]))]
      (-> state
          (update-in [:bot bot] remove-val low)
          (update-in [:bot bot] remove-val high)
          (update-in (first rule) conj low)
          (update-in (second rule) conj high)))
    state))

(defn part1
  [input chips]
  (->> (initial-state input)
       (iterate (partial step (bot-rules input)))
       (drop-while (fn [state]
                     (not= chips
                           (set (get-in state [:bot (ready-bot state)])))))
       first
       ready-bot))

(defn part2
  [input outputs]
  (let [m (->> (initial-state input)
               (iterate (partial step (bot-rules input)))
               (drop-while #(pos? (apply + (map (comp count val) (:bot %)))))
               first
               :output)]
    (->> (select-keys m outputs)
         vals
         (map first)
         (apply *))))

(part1 input #{17 61}) ;; => 113
(part2 input [0 1 2])  ;; => 12803
