(ns aoc-2020.day-24
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 24:  --- https://adventofcode.com/2020/day/24

(def task-input (u/slurp-resource "inputs/aoc_2020/day-24.txt"))

(def test-input "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew")


(defn parse-instructions
  [line]
  (loop [[^char a & [^char b & rest-2 :as rest-1]] line
         directions []]
    (if (nil? a)
      directions
      (case a
        \e (recur rest-1 (conj directions [1 0]))
        \w (recur rest-1 (conj directions [-1 0]))
        \n (case b
             \e (recur rest-2 (conj directions [0 -1]))
             \w (recur rest-2 (conj directions [-1 -1])))
        \s (case b
             \e (recur rest-2 (conj directions [1 1]))
             \w (recur rest-2 (conj directions [0 1])))))))


(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map parse-instructions)))


(defn calculate-offset
  [instructions]
  (->> instructions
       (reduce
         (fn [[x y :as coords] [ox oy]]
           (-> coords
               (assoc! 0 (+ x ox))
               (assoc! 1 (+ y oy))))
         (transient [0 0]))
       persistent!))


(defn part-1
  [input]
  (->> (parse-input input)
       (map calculate-offset)
       frequencies
       vals
       (remove #(= 0 (mod % 2)))
       count))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10
  (part-1 task-input)                                       ; => 287

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
