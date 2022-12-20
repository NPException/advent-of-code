(ns aoc-2022.day-17
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 17: Pyroclastic Flow ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-17.txt"))

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")


; NOTE: Y+ is upwards in this coordinate system

(def empty-sprite \.)

(def empty-grid-line
  (vec (repeat 7 empty-sprite)))

(def start-grid
  [(vec "-------")])

(defn at
  [grid x y]
  (u/nth-in grid [y x] empty-sprite))


(defn build-free-predicate
  [rock-pattern]
  (let [width (count (first rock-pattern))]
    (->> rock-pattern
         (map-indexed
           (fn [oy row]
             (keep-indexed
               (fn [ox sprite]
                 (when (not= sprite empty-sprite)
                   (fn [grid x y]
                     (and (<= 0 x (- 7 width))
                          (= empty-sprite (at grid (+ x ox) (+ y oy)))))))
               row)))
         (apply concat)
         (apply u/and-fn))))


(defn ensure-height
  [grid$ height]
  (let [d (- height (count grid$))]
    (if (<= d 0)
      grid$
      (reduce conj! grid$ (repeat d empty-grid-line)))))


(defn build-place-function
  [rock-pattern]
  (let [height (count rock-pattern)
        width  (count (first rock-pattern))]
    (fn [grid x y sprite]
      (loop [oy    0, ox 0
             grid$ (-> (transient grid)
                       (ensure-height (+ y (count rock-pattern))))
             row$  (transient (nth grid$ y))]
        (cond
          ; finished
          (= oy height)
          (persistent! grid$)
          ; new row
          (= ox width)
          (recur (inc oy) 0
            (assoc! grid$ (+ y oy) (persistent! row$))
            (some->> (nth grid$ (+ y (inc oy)) nil) transient))
          ; next x
          :else
          (recur oy (inc ox)
            grid$
            (if (= \# (u/nth-in rock-pattern [oy ox]))
              (assoc! row$ (+ x ox) sprite)
              row$)))))))


(defn parse-rock
  [s]
  (let [pattern (->> (str/split-lines s)
                     (mapv (fn [line]
                             (mapv #(or (#{\#} %) empty-sprite) line)))
                     ; reverse because Y+ is upwards
                     (rseq)
                     (vec))]
    [pattern
     ; predicate that determines if a spot in the grid that this rock want's to sit in is available
     (build-free-predicate pattern)
     ; function that adds this rock to the grid at the given place
     (build-place-function pattern)]))


(def rocks
  (->> (str/split "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##" #"\n\n")
       (mapv parse-rock)
       (cycle)))


(defn highest-point
  [grid]
  (loop [y (dec (count grid))]
    (if (some #(not= empty-sprite %) (nth grid y))
      y
      (recur (dec y)))))


(defn print-grid
  [grid]
  ; reverse because Y+ is upwards
  (doseq [row (rseq grid)]
    (println (str \| (apply str row) \|)))
  (println))


(defn rock-spawn-position
  [grid]
  (let [top (highest-point grid)
        y   (+ top 4)]
    [2 y]))


(defn push
  [jet x]
  (if (= jet \>) (inc x) (dec x)))


(defn part-1
  [input]
  (loop [[jet & jets] (cycle input)
         [[_ free? place] :as rocks] rocks
         rock-count 0
         grid       start-grid
         [x y] (rock-spawn-position grid)]
    (if (= rock-count 2022)
      (highest-point grid)
      (do #_(print-grid (place grid x y \@))
          (let [nx (push jet x)
                nx (if (free? grid nx y) nx x)]
            #_(println jet)
            #_(print-grid (place grid nx y \@))
            (if (free? grid nx (dec y))
              (recur jets rocks rock-count grid [nx (dec y)])
              (let [grid' (place grid nx y \#)]
                (recur
                  jets
                  (next rocks)
                  (inc rock-count)
                  grid'
                  (rock-spawn-position grid')))))))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 3068
  (part-1 task-input)                                       ; => 3135
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
