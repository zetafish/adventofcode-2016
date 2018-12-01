(ns advent.day4
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def x1 "aaaaa-bbb-z-y-x-123[abxyz]")
(def x2 "a-b-c-d-e-f-g-h-987[abcde]")
(def x3 "not-a-real-room-404[oarel]")
(def x4 "totally-real-room-200[decoy]")
(def x5 "qzmt-zixmtkozy-ivhz-343[zimth]")

(decrypt (parse x5))


(def input
  (str/split-lines (slurp "input/input4.txt")))

(insta/defparser p
  "<S>      = name <'-'> sector <'['> checksum <']'>
   name     = letter+('-' letter+)*
   sector   = digit+
   checksum = letter*
   <letter> = #'[a-z]'
   <digit>  = #'[0-9]'")

(defn parse
  [s]
  (let [[name sector checksum] (p s)]
    {:name (str/join (rest name))
     :sector (Integer/parseInt (str/join (rest sector)))
     :checksum (str/join (rest checksum))}))

(defn crc
  [name]
  (->> (frequencies name)
       (filter #(not= \- (first %)))
       (sort-by (juxt (comp #(- %) second) first))
       (map first)
       (take 5)
       (str/join)))

(defn real-room?
  [room]
  (let [{:keys [name checksum]} room]
    (= checksum (crc name))))

(defn rotate
  [c n]
  (condp = c
    \- \space
    \space \space
    (char (+ (int \a)
             (mod (+ n (- (int c) (int \a)))
                  26)))))

(defn decrypt
  [{:keys [name sector] :as room}]
  (assoc room :decrypted (str/join
                           (map #(rotate % sector) name))))

(defn part1
  [input]
  (->> input
       (map parse)
       (filter real-room?)
       (map :sector)
       (apply +)))

(defn part2
  [input]
  (->>
    input
    (map parse)
    (filter real-room?)
    (map decrypt)
    (filter #(str/includes? (:decrypted %) "pole"))))

(part1 input)
(part2 input)
