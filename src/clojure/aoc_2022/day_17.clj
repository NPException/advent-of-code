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
  (dec (count grid)))


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


(defn simulate
  [input rocks-to-simulate]
  (loop [[jet & jets] (cycle input)
         [[_ free? place] :as rocks] rocks
         rock-count 0
         grid       start-grid
         peaks$     (transient [0])
         [x y] (rock-spawn-position grid)]
    (if (= rock-count rocks-to-simulate)
      [grid (persistent! peaks$)]
      (do #_(print-grid (place grid x y \@))
        (let [nx (push jet x)
              nx (if (free? grid nx y) nx x)]
          #_(println jet)
          #_(print-grid (place grid nx y \@))
          (if (free? grid nx (dec y))
            (recur jets rocks rock-count grid peaks$ [nx (dec y)])
            (let [grid' (place grid nx y \#)]
              (recur
                jets
                (next rocks)
                (inc rock-count)
                grid'
                (conj! peaks$ (highest-point grid))
                (rock-spawn-position grid')))))))))


(defn part-1
  [input]
  (-> (simulate input 2022)
      (first)
      (highest-point)))


(defn last-index-while
  [pred coll]
  (->> (take-while pred coll)
       (count)
       (dec)))


(defn part-2
  [input]
  (let [jet-count          (count input)
        sim-rock-nr        (inc (* 5 jet-count))
        [grid peaks] (simulate input sim-rock-nr)
        g                  (subvec grid (* 2 jet-count))
        cycle-height       (->> (range jet-count (quot (count g) 2))
                                (keep (fn [n]
                                        (when (->> (partition n g)
                                                   (take 2)
                                                   (apply =))
                                          n)))
                                (first))
        first-cycle-start  (->> (range)
                                (filter (fn [i]
                                          (->> (drop i grid)
                                               (partition cycle-height)
                                               (take 2)
                                               (apply =))))
                                (first))
        first-cycle-end    (+ first-cycle-start cycle-height)
        rocks-before-cycle (dec (last-index-while #(<= % first-cycle-start) peaks))
        rocks-after-cycle  (dec (last-index-while #(<= % first-cycle-end) peaks))
        rocks-per-cycle    (- rocks-after-cycle rocks-before-cycle)
        nr-of-cycles       (quot 1000000000000 rocks-per-cycle)]
    (+ (dec first-cycle-start) (* nr-of-cycles cycle-height))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 3068
  (part-1 task-input)                                       ; => 3135
  (crit/quick-bench (part-1 task-input))

  ; FIXME: code works for example but not for task input :(
  ;; Part 2
  (part-2 test-input)                                       ; => 1514285714288
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
