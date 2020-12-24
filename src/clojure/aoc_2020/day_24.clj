(ns aoc-2020.day-24
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [aoc-utils :as u]))

;; --- Day 24: Lobby Layout --- https://adventofcode.com/2020/day/24

(def task-input (u/slurp-resource "inputs/aoc_2020/day-24.txt"))

(def test-input "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew")


(defn parse-instructions
  [line]
  (loop [[^char a & [^char b & rest-2 :as rest-1]] line
         [x y :as coordinate] [0 0]]
    (if (nil? a)
      coordinate
      (match
        [a b]
        [\e _] (recur rest-1 [(inc x) y])
        [\w _] (recur rest-1 [(dec x) y])
        [\n \e] (recur rest-2 [(inc x) (dec y)])
        [\n \w] (recur rest-2 [x (dec y)])
        [\s \e] (recur rest-2 [x (inc y)])
        [\s \w] (recur rest-2 [(dec x) (inc y)])))))


(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map parse-instructions)))


(defn flip
  [+active-tiles coord]
  (if (+active-tiles coord)
    (disj! +active-tiles coord)
    (conj! +active-tiles coord)))


(defn create-initial-grid
  [input]
  (->> (parse-input input)
       (reduce flip (transient #{}))
       persistent!))


(defn part-1
  [input]
  (count (create-initial-grid input)))


;; part 2

(def adjacent-offsets
  [[1 0] [1 -1] [0 -1] [-1 0] [-1 1] [0 1]])

(defn inc-safe
  [x]
  (inc (or x 0)))


(defn active->inactive
  [active-tiles [+nearby-actives +new-active-tiles] [x y :as active-tile]]
  (let [adjacent-inactives (->> adjacent-offsets
                             (map (fn [[ox oy]] [(+ x ox) (+ y oy)]))
                             (remove active-tiles))
        adjacent-actives (- 6 (count adjacent-inactives))]
    [(reduce
       #(u/update! %1 %2 inc-safe)
       +nearby-actives
       adjacent-inactives)
     (if (or (= adjacent-actives 0)
             (> adjacent-actives 2))
       (disj! +new-active-tiles active-tile)
       +new-active-tiles)]))


(defn inactive->active
  [+new-active-tiles [inactive-tile adjacent-actives]]
  (if (= 2 adjacent-actives)
    (conj! +new-active-tiles inactive-tile)
    +new-active-tiles))


(defn daily-flips
  [active-tiles]
  (let [[+inactive-adjacent-actives +new-active-tiles]
        (reduce
          (partial active->inactive active-tiles)
          [(transient {}) (transient active-tiles)]
          active-tiles)]
    (persistent!
      (reduce
        inactive->active
        +new-active-tiles
        (persistent! +inactive-adjacent-actives)))))


(defn part-2
  [input]
  (let [active-tiles (create-initial-grid input)]
    (->> active-tiles
         (iterate daily-flips)
         (drop 100)
         first
         count)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10
  (part-1 task-input)                                       ; => 287

  ;; Part 2
  (part-2 test-input)                                       ; => 2208
  (part-2 task-input)                                       ; => 3636

  )
