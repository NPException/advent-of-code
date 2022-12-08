(ns aoc-2022.day-8
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 8: Treetop Tree House ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-8.txt"))

(def test-input "30373\n25512\n65332\n33549\n35390")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv (fn [line]
               (mapv #(- (int %) (int \0)) line)))))


(defn at
  ^long [grid x y]
  (u/nth-in grid [y x]))


(defn visible?
  [grid width height x y]
  (let [^long width  width
        ^long height height
        ^long x      x
        ^long y      y
        tree         (at grid x y)]
    (not
      (and (loop [l 0]
             (when (< l x)
               (or (>= (at grid l y) tree)
                   (recur (inc l)))))
           (loop [r (dec width)]
             (when (> r x)
               (or (>= (at grid r y) tree)
                   (recur (dec r)))))
           (loop [u 0]
             (when (< u y)
               (or (>= (at grid x u) tree)
                   (recur (inc u)))))
           (loop [d (dec height)]
             (when (> d y)
               (or (>= (at grid x d) tree)
                   (recur (dec d)))))))))


(defn part-1
  [input]
  (let [grid       (parse-input input)
        height     (count grid)
        width      (count (first grid))
        edge-count (-> (dec height)
                       (+ (dec width))
                       (* 2))]
    (+ edge-count
       (count (for [x (range 1 (dec width))
                    y (range 1 (dec height))
                    :when (visible? grid width height x y)]
                true)))))


(defn scenic-score
  [grid max-x max-y x y]
  (let [^long max-x max-x
        ^long max-y max-y
        ^long x     x
        ^long y     y
        tree        (at grid x y)]
    (* (loop [l (dec x)]
         (if (and (> l 0) (> tree (at grid l y)))
           (recur (dec l))
           (- x l)))
       (loop [u (dec y)]
         (if (and (> u 0) (> tree (at grid x u)))
           (recur (dec u))
           (- y u)))
       (loop [r (inc x)]
         (if (and (< r max-x) (> tree (at grid r y)))
           (recur (inc r))
           (- r x)))
       (loop [d (inc y)]
         (if (and (< d max-y) (> tree (at grid x d)))
           (recur (inc d))
           (- d y))))))


(defn part-2
  [input]
  (let [grid  (parse-input input)
        max-y (dec (count grid))
        max-x (dec (count (first grid)))]
    (->> (for [x (range 1 max-x)
               y (range 1 max-y)]
           (scenic-score grid max-x max-y x y))
         (apply max))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 21
  (part-1 task-input)                                       ; => 1794
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 8
  (part-2 task-input)                                       ; => 199272
  (crit/quick-bench (part-2 task-input))

  )
