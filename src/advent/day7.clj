(ns advent.day7
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.set :as set]))

(def x1 "abba[mnop]qrst")
(def x2 "abcd[bddb]xyyx")
(def x3 "aaaa[qwer]tyui")
(def x4 "ioxxoj[asdfgh]zxcvbn")

(def x5 "zazbz[bzb]cdb")

(def input
  (str/split-lines (slurp "input/input7.txt")))

(insta/defparser p
  "<S> = supernet (<'['> hypernet <']'> supernet)*
   supernet = #'[a-z]*'
   hypernet = #'[a-z]*'")

(defn parse
  [ip]
  (p ip))

(defn windows
  [n s]
  (distinct
    (mapcat #(partition n (drop % s))
            (range (- (count s) (dec n))))))

(defn abba
  [s]
  (letfn [(p [[a b c d]]
            (and (= a d) (= b c) (not= a b)))]
    (filter p (windows 4 s))))

(defn aba
  [s]
  (letfn [(p [[a b c]]
            (and (= a c) (not= a b)))]
    (filter p (windows 3 s))))

(defn find-pattern
  [net pat ip]
  (->> ip
       (filter #(= net (first %)))
       (mapcat (comp pat second))))

(defn tls?
  [ip]
  (and
    (seq (find-pattern :supernet abba ip))
    (not (seq (find-pattern :hypernet abba ip)))))

(defn ssl?
  [ip]
  (letfn [(flip [[a b a]] [b a b])]
    (seq
      (set/intersection
        (->> (find-pattern :supernet aba ip)
             (map flip)
             set)
        (->> (find-pattern :hypernet aba ip)
             set)))))

(defn part1
  [input]
  (->> (map parse input)
       (filter tls?)
       (count)))

(defn part2
  [input]
  (->> (map parse input)
       (filter ssl?)
       (count)))

(part1 input) ;; => 110

(part2 input) ;; => 242
