(ns aoc-2023.day-14
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 14: Parabolic Reflector Dish ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-14.txt"))

(def test-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")


(defn slide-column
  [grid x]
  (let [height (count grid)]
    (loop [grid grid
           low  0
           y    0]
      (if (= y height)
        grid
        (case (u/nth-in grid [y x])
          \. (recur grid low (inc y))
          \# (recur grid (inc y) (inc y))
          \O (recur
               (-> grid
                   (assoc-in [y x] \.)
                   (assoc-in [low x] \O))
               (inc low)
               (inc y)))))))

(defn slide-rocks
  [grid]
  (let [width  (count (first grid))
        height (count grid)
        xs     (range width)
        ys     (range height)]
    (reduce
      slide-column
      grid
      xs)))


(defn column-load
  [grid x]
  (let [height (count grid)]
    (->> (range height)
         (filterv #(= \O (u/nth-in grid [% x])))
         (mapv #(- height %))
         (apply +))))


(defn calculate-load
  [grid]
  (->> (range (count (first grid)))
       (mapv #(column-load grid %))
       (apply +)))


(defn part-1
  [input]
  (->> (str/split-lines input)
       (mapv vec)
       (slide-rocks)
       (calculate-load)))


(defn rotate-grid-clockwise
  [grid]
  (let [xs  (range (count (first grid)))
        rys (reverse (range (count grid)))]
    (->> xs
         (mapv
           (fn [x]
             (mapv #(u/nth-in grid [% x]) rys))))))


(defn part-2
  [input]
  (let [grid        (->> (str/split-lines input)
                         (mapv vec))
        slide-step  #(-> % (slide-rocks) (rotate-grid-clockwise))
        slide-cycle #(-> % (slide-step) (slide-step) (slide-step) (slide-step))]
    (loop [step 0
           prev-grid grid
           seen {grid step}]
      (let [grid (slide-cycle prev-grid)]
        (if-let [last-seen (seen grid)]
          (let [repeat-size (- step last-seen)
                steps-needed (mod (- 1000000000 last-seen) repeat-size)]
            (->> (iterate slide-cycle grid)
                 (take steps-needed)
                 (last)
                 (calculate-load)))
          (recur (inc step) grid (assoc seen grid step)))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 136
  (part-1 task-input)                                       ; => 109098
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 64
  (part-2 task-input)                                       ; => 100064
  (crit/quick-bench (part-2 task-input))

  )
