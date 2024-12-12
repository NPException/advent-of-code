(ns aoc-2024.day-12
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit])
  (:import (java.util HashSet)))

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

(defn analyse-plant
  "Returns the perimeter and neighbours of the plant"
  [plant-grid                                               ; original grid of plants
   ^HashSet seen-coordinates                                ; set of already seen coordinates
   [^long x ^long y plant]]                                 ; the plant to analyze
  (.add seen-coordinates [x y])
  (->> neighbour-offsets
       (reduce
         (fn [[^long perimeter neighbours] [^long dx ^long dy]]
           (let [nx (+ x dx)
                 ny (+ y dy)
                 n-plant (u/nth-in plant-grid [ny nx] nil)]
             (if-not (= plant n-plant)
               ; if it's not the same plant, +1 to perimeter and continue
               [(inc perimeter) neighbours]
               ; if it's the same plant ...
               (if (.contains seen-coordinates [nx ny])
                 ; ... and was already seen, we skip it
                 [perimeter neighbours]
                 ; else we add it to the neighbours and mark it as seen.
                 (do (.add seen-coordinates [nx ny])
                     [perimeter (conj neighbours [nx ny n-plant])])))))
         [0 []])))

(defn price-region
  [plant-grid
   ^HashSet seen-coordinates
   [^long x ^long y _ :as plot]]
  (if (.contains seen-coordinates [x y])
    nil
    (let [[^long perimeter neighbours] (analyse-plant plant-grid seen-coordinates plot)]
      (loop [total-perimeter perimeter
             total-area 1
             [neighbour & more] neighbours]
        (if (nil? neighbour)
          (* total-perimeter total-area)
          (let [[^long n-perim n-neigbours] (analyse-plant plant-grid seen-coordinates neighbour)]
            (recur
              (+ total-perimeter n-perim)
              (inc total-area)
              (into n-neigbours more))))))))


(defn part-1
  [input]
  (let [grid (parse-grid input)
        seen (HashSet/newHashSet (count input))]
    (->> (u/grid-elements grid)
         (mapv #(price-region grid seen %))
         (filterv some?)
         (apply +))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1930
  (part-1 task-input)                                       ; => 1434856
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 1206
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
