(ns aoc-2020.day-24
  (:require [clojure.string :as string]
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
      (case a
        \e (recur rest-1 [(inc x) y])
        \w (recur rest-1 [(dec x) y])
        \n (case b
             \e (recur rest-2 [(inc x) (dec y)])
             \w (recur rest-2 [x (dec y)]))
        \s (case b
             \e (recur rest-2 [x (inc y)])
             \w (recur rest-2 [(dec x) (inc y)]))))))


(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map parse-instructions)))


(defn flip
  [+black-tiles coord]
  (if (+black-tiles coord)
    (disj! +black-tiles coord)
    (conj! +black-tiles coord)))


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


(defn black->white
  [black-tiles [+white-adjacent-blacks +new-black-tiles] [x y :as black-tile]]
  (let [adjacent-whites (->> adjacent-offsets
                             (map (fn [[ox oy]] [(+ x ox) (+ y oy)]))
                             (remove black-tiles))
        adjacent-blacks (- 6 (count adjacent-whites))]
    [(reduce
       #(u/update! %1 %2 inc-safe)
       +white-adjacent-blacks
       adjacent-whites)
     (if (or (= adjacent-blacks 0)
             (> adjacent-blacks 2))
       (disj! +new-black-tiles black-tile)
       +new-black-tiles)]))


(defn white->black
  [+new-black-tiles [white-tile adjacent-blacks]]
  (if (= 2 adjacent-blacks)
    (conj! +new-black-tiles white-tile)
    +new-black-tiles))


(defn daily-flips
  [black-tiles]
  (let [[+white-adjacent-blacks +new-black-tiles]
        (reduce
          (partial black->white black-tiles)
          [(transient {}) (transient black-tiles)]
          black-tiles)]
    (persistent!
      (reduce
        white->black
        +new-black-tiles
        (persistent! +white-adjacent-blacks)))))


(defn part-2
  [input]
  (let [black-tiles (create-initial-grid input)]
    (->> black-tiles
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
