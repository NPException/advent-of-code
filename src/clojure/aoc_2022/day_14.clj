(ns aoc-2022.day-14
  (:require [aoc-utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [criterium.core :as crit]
            [image-utils :as img]))

;; --- Day 14: Regolith Reservoir ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-14.txt"))

(def test-input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

(def ^:const spawn -1)
(def ^:const air 0)
(def ^:const rock 1)
(def ^:const sand 2)

(defn print-grid
  [grid]
  (let [width     (alength ^ints (aget ^objects grid 0))
        separator (apply str (repeat width \-))]
    (doseq [row grid]
      (println (apply str (map {air   \space
                                rock  \█
                                sand  \░
                                spawn \+}
                            row))))
    (println separator)))

(defn at
  ^long [^objects grid ^long x ^long y]
  (if (>= y (alength grid))
    air
    (let [^ints row (aget grid 0)
          width     (alength row)]
      (if (or (< x 0) (>= x width))
        air
        (aget ^ints (aget ^objects grid y) x)))))

(defn place!
  ^long [grid ^long x ^long y ^long value]
  (aset-int (aget ^objects grid y) x (int value)))

(defn parse-input
  [input with-floor?]
  (let [spawn-point [500 0]
        paths       (->> (str/split-lines input)
                         (mapv (fn [line]
                                 (->> (str/split line #" -> ")
                                      (mapv #(u/split-parse % parse-long #"," parse-long))))))
        [xs ys] (->> (apply concat [spawn-point] paths)
                     (reduce
                       (fn [[xs ys] [x y]]
                         [(conj xs x) (conj ys y)])
                       [#{} #{}]))
        min-x       (apply min xs)
        max-x       (apply max xs)
        min-y       (apply min ys)
        max-y       (apply max ys)
        width       (inc (- max-x min-x))
        height      (inc (- max-y min-y))
        ;; adjust values based on part 2 condition
        height      (if with-floor?
                      (+ height 3)
                      height)
        x-margin    (if with-floor? height 0)
        width       (+ width x-margin x-margin)
        x-offset    (- x-margin min-x)
        y-offset    (- min-y)
        grid        (make-array Integer/TYPE height width)]
    ;; add spawn-point to grid
    (place! grid (+ 500 x-offset) (- 0 y-offset) spawn)
    ;; add floor if desired
    (when with-floor?
      (let [floor-level (+ max-y 2)]
        (dotimes [x width]
          (place! grid x floor-level rock))))
    ;; add walls to grid
    (doseq [[[x0 y0] [x1 y1]] (mapcat #(partition 2 1 %) paths)]
      (let [x0 (+ x0 x-offset)
            x1 (+ x1 x-offset)
            y0 (+ y0 y-offset)
            y1 (+ y1 y-offset)
            xs (min x0 x1)
            xe (inc (max x0 x1))
            ys (min y0 y1)
            ye (inc (max y0 y1))]
        (dotimes [yo (- ye ys)]
          (dotimes [xo (- xe xs)]
            (place! grid
              (+ xs xo) (+ ys yo)
              rock)))))
    ;; return spawn coordinates in grid, and the grid itself
    [[(+ 500 x-offset)
      (+ 0 y-offset)]
     grid]))


(defn spawn-sand!
  [^objects grid ^long sx ^long sy]
  (when (== spawn (at grid sx sy))
    (let [height (alength grid)
          width  (alength ^ints (aget grid 0))]
      (loop [x      sx
             next-y (inc sy)]
        (cond
          ;; fell out of bounds
          (or (< x 0) (>= x width) (>= next-y height))
          false
          ;; can fall down
          (== air (at grid x next-y))
          (recur x (inc next-y))
          ;; can fall left
          (== air (at grid (dec x) next-y))
          (recur (dec x) (inc next-y))
          ;; can fall right
          (== air (at grid (inc x) next-y))
          (recur (inc x) (inc next-y))
          ;; nowhere to go
          :else
          (do (place! grid x (dec next-y) sand)
              true))))))


(defn store-image
  [^objects grid name]
  (.mkdirs (io/file "visualizations/aoc_2022/day-14/"))
  (img/write-png! (str "visualizations/aoc_2022/day-14/" name ".png")
    (img/image-from-data
      (fn [x]
        (condp = x
          air [0.0 0.0 0.0]
          spawn [1.0 0.0 0.0]
          rock [0.5 0.5 0.5]
          sand (let [c (+ 0.7 (rand 0.2))]
                 [c c 0])))
      10.0
      (let [height (alength grid)
            width  (alength ^ints (aget grid 0))]
        (->> (range height)
             (mapv (fn [y]
                     (mapv #(at grid % y) (range width)))))))))


(defn simulate
  [input with-floor? name]
  (let [[[sx sy] grid] (parse-input input with-floor?)]
    (loop [spawned 0]
      (if (spawn-sand! grid sx sy)
        (do
          ;(print-grid grid)
          (recur (inc spawned)))
        (do (store-image grid name)
            spawned)))))


(defn part-1
  [input name]
  (simulate input false name))


(defn part-2
  [input name]
  (simulate input true name))


(comment
  ;; Part 1
  (part-1 test-input "test_1")                              ; => 24
  (part-1 task-input "task_1")                              ; => 862
  (crit/quick-bench (part-1 task-input "task_1"))

  ;; Part 2
  (part-2 test-input "test_2")                              ; => 93
  (part-2 task-input "task_2")                              ; => 28744
  (crit/quick-bench (part-2 task-input "task_2"))

  )
