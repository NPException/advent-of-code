(ns aoc-2016.day-4
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 4: Security Through Obscurity ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-4.txt"))

(def test-input "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]")


(defn parse-room
  [room-string]
  (let [[_ name sector-id checksum] (re-find #"(\D+)-(\d+)\[(.+?)\]" room-string)]
    [name (parse-long sector-id) checksum]))


(defn gen-checksum
  [letters]
  (->> letters
       (filter #(Character/isLetter ^Character %))
       (frequencies)
       (sort-by (juxt #(- (val %)) key))
       (map key)
       (take 5)
       (apply str)))

(defn real-room?
  [[name _sector-id checksum]]
  (= checksum (gen-checksum name)))


(defn real-rooms
  [input]
  (->> (str/split-lines input)
       (map parse-room)
       (filter real-room?)))


(defn part-1
  [input]
  (->> (real-rooms input)
       (map second)
       (apply +)))


(def offset (int \a))

(defn shift
  [letter n]
  (-> (int letter)
      (- offset)
      (+ n)
      (mod 26)
      (+ offset)
      (char)))


(defn decrypt-name
  [[name sector-id]]
  [(->> name
        (mapv #(if (= % \-) " " (shift % sector-id)))
        (apply str))
   sector-id])


(defn part-2
  [input]
  (->> (real-rooms input)
       (map decrypt-name)
       (u/first-match #(str/includes? (first %) "northpole"))
       (second)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1514
  (part-1 task-input)                                       ; => 185371
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 task-input)                                       ; => 984
  (crit/quick-bench (part-2 task-input))

  )
