(ns aoc-2024.day-12
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit])
  (:import (java.util HashMap)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 12: Garden Groups ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-12.txt"))

(def test-input "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE")

(defn parse-grid
  [text]
  (str/split-lines text))


(def neighbour-offsets
  ; right, bottom, left, top
  [[1 0] [0 1] [-1 0] [0 -1]])

(defn analyse-plant!
  "Sets the region for the given plant,
   and returns same-plant neighbours that don't have a region yet."
  [plant-grid                                               ; original grid of plants
   region-grid                                              ; 2d array of region-id per coordinate. id of 0 indicates no region yet.
   ^long region-id                                          ; id of the region that is currently being filled
   [^long x ^long y plant]]                                 ; the plant to analyze
  (aset-long region-grid y x region-id)
  (->> neighbour-offsets
       (u/keepv
         (fn [[^long dx ^long dy]]
           (let [nx (+ x dx)
                 ny (+ y dy)
                 n-plant (u/nth-in plant-grid [ny nx] nil)]
             ; if neighbour is the same plant and hasn't been seen before (region is not already set)
             (when (and (= plant n-plant)
                        (zero? (long (u/nth-in region-grid [ny nx]))))
               [nx ny n-plant]))))))

(defn compute-region!
  [plant-grid
   region-grid
   generate-region-id
   [^long x ^long y _ :as plot]]
  ; only compute if this spot doesn't belong to a region already
  (when (zero? (long (u/nth-in region-grid [y x])))
    ; else continue analysis
    (let [region-id (generate-region-id)
          neighbours (analyse-plant! plant-grid region-grid region-id plot)]
      (loop [[neighbour & more] neighbours]
        (when neighbour
          (let [n-neighbours (analyse-plant! plant-grid region-grid region-id neighbour)]
            (recur (into n-neighbours more))))))))


(defn top-boundary?
  [grid [^long x ^long y ^long id]]
  (not (= id (long (u/nth-in grid [(dec y) x] 0)))))

(defn bottom-boundary?
  [grid [^long x ^long y ^long id]]
  (not (= id (long (u/nth-in grid [(inc y) x] 0)))))

(defn left-boundary?
  [grid [^long x ^long y ^long id]]
  (not (= id (long (u/nth-in grid [y (dec x)] 0)))))

(defn right-boundary?
  [grid [^long x ^long y ^long id]]
  (not (= id (long (u/nth-in grid [y (inc x)] 0)))))


(defn add-segment!
  [^HashMap perimeters segment-id ^long length]
  (when segment-id
    (.compute perimeters segment-id
      (fn [_ old]
        (+ (long (or old 0)) length)))))

(defn analyse-line-boundaries
  [^HashMap perimeters
   grid line
   boundary? segment-length-fn]
  (let [length (count line)
        [_ _ id1 :as e1] (nth line 0)
        first-boundary? (boundary? grid e1)]
    (loop [idx 1
           segment-id (when first-boundary? id1)
           parts (long (if first-boundary? 1 0))]
      (if (>= idx length)
        ; done
        (add-segment! perimeters segment-id (segment-length-fn parts))
        ; walk line
        (let [[_ _ id :as e] (nth line idx)
              e-boundary? (boundary? grid e)]
          (if (and (= id segment-id) e-boundary?)
            ; element is part of continuous boundary segment
            (recur (inc idx) segment-id (inc parts))
            ; element is not part of continuous boundary
            (do (add-segment! perimeters segment-id (segment-length-fn parts))
                (recur (inc idx) (when e-boundary? id) (long (if e-boundary? 1 0))))))))))

(defn analyse-regions
  [region-grid segment-length-fn]
  (let [width (count (first region-grid))
        height (count region-grid)
        areas (->> (u/grid-elements region-grid)
                   (mapv #(nth % 2))
                   (frequencies))
        perimeters (HashMap.)
        ys (vec (range 0 height))
        xs (vec (range 0 width))]
    ; analyse top & bottom boundaries
    (doseq [y ys
            :let [line (mapv #(u/grid-element region-grid % y) xs)]]
      (analyse-line-boundaries perimeters region-grid line top-boundary? segment-length-fn)
      (analyse-line-boundaries perimeters region-grid line bottom-boundary? segment-length-fn))
    ; analyse left & right boundaries
    (doseq [x xs
            :let [line (mapv #(u/grid-element region-grid x %) ys)]]
      (analyse-line-boundaries perimeters region-grid line left-boundary? segment-length-fn)
      (analyse-line-boundaries perimeters region-grid line right-boundary? segment-length-fn))
    ; calculate prices
    (reduce-kv
      (fn [^long price id ^long area]
        (+ price (* area ^long (.get perimeters id))))
      0
      areas)))


(defn solve
  [input price-per-segment-fn]
  (let [grid (parse-grid input)
        region-grid (make-array Long/TYPE (count grid) (count (first grid)))
        next-id (atom 0)
        region-id-generator #(swap! next-id inc)]
    ; fill region-grid
    (doseq [plot (u/grid-elements grid)]
      (compute-region! grid region-grid region-id-generator plot))
    (analyse-regions region-grid price-per-segment-fn)))


(defn part-1
  [input]
  (solve input identity))


(defn part-2
  [input]
  (solve input (constantly 1)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1930
  (part-1 task-input)                                       ; => 1434856
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 1206
  (part-2 task-input)                                       ; => 891106
  (crit/quick-bench (part-2 task-input))

  )
