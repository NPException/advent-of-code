(ns aoc-2022.day-18
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 18: Boiling Boulders ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-18.txt"))

(def test-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")

(defn parse-input
  [input]
  (->> (u/read-as-vector input)
       (u/vpartition 3)
       (set)))


(def offsets
  ;x-       x+       y-       y+       z-       z+
  (->> [[-1 0 0] [1 0 0], [0 -1 0] [0 1 0], [0 0 -1] [0 0 1]]
       (mapv (fn [[ox oy oz]]
               (fn [[x y z]]
                 [(+ ox x) (+ oy y) (+ oz z)])))))


(defn count-open-sides
  [cubes cube]
  (- 6
     (u/count-matching
       (fn [offset-fn]
         (cubes (offset-fn cube)))
       offsets)))


(defn part-1
  [input]
  (let [cubes (parse-input input)]
    (reduce
      (fn [n cube]
        (+ n (count-open-sides cubes cube)))
      0
      cubes)))



(defn find-limits
  [cubes]
  (reduce
    (fn [[x- x+ y- y+ z- z+] [x y z]]
      [(min x- x) (max x+ x)
       (min y- y) (max y+ y)
       (min z- z) (max z+ z)])
    [Long/MAX_VALUE Long/MIN_VALUE
     Long/MAX_VALUE Long/MIN_VALUE
     Long/MAX_VALUE Long/MIN_VALUE]
    cubes))


(defn part-2
  [input]
  (let [cubes         (parse-input input)
        [x- x+ y- y+ z- z+] (find-limits cubes)
        x-            (dec x-)
        x+            (inc x+)
        y-            (dec y-)
        y+            (inc y+)
        z-            (dec z-)
        z+            (inc z+)
        neighbours-fn (fn [pos]
                        (->> (mapv #(% pos) offsets)
                             (filterv (fn [[x y z]]
                                        (and (<= x- x x+)
                                             (<= y- y y+)
                                             (<= z- z z+))))))]
    ; flood fill
    (loop [unchecked     #{[x- y- z-]}
           checked       cubes
           exposed-faces 0]
      (if (empty? unchecked)
        exposed-faces
        (let [point         (first unchecked)               ; grab next point to fill from
              checked       (conj checked point)            ; add it to the checked set
              need-to-check (disj unchecked point)          ; remove it from the set of unchecked points
              neighbours    (neighbours-fn point)           ; find neigbours of the point
              exposed-faces (+ exposed-faces (u/count-matching cubes neighbours)) ; add any neighbours that are in the input cubes to the exposed faces counter
              unchecked     (into need-to-check (remove checked neighbours))] ; add yet unchecked neighbours to the unchecked set
          (recur unchecked checked exposed-faces))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 64
  (part-1 task-input)                                       ; => 4460
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 58
  (part-2 task-input)                                       ; => 2498
  (crit/quick-bench (part-2 task-input))

  )
