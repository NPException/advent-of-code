(ns aoc-2023.day-5
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 5: If You Give A Seed A Fertilizer ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-5.txt"))

(def test-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")


(defn create-range
  [input-line]
  (let [[^long dest ^long src ^long len] (u/split-parse input-line parse-long #" " parse-long #" " parse-long)
        diff    (- dest src)
        end-src (+ src len)]
    (fn [^long x]
      (and (>= x src)
           (< x end-src)
           (+ x diff)))))

(defn create-mapping
  [mapping-input]
  (let [ranges (->> (str/split-lines mapping-input)
                    (drop 1)
                    (map create-range))]
    (fn [x]
      (or (some #(% x) ranges)
          x))))


(defn parse-input
  [input]
  (let [[seed-line & mappings] (str/split input #"\n\n")]
    {:seeds   (first (u/split-parse seed-line nil #"seeds: " u/read-as-vector))
     :mapping (->> mappings
                   (map create-mapping)
                   (reverse)
                   (apply comp))}))


(defn part-1
  [input]
  (let [{:keys [seeds mapping]} (parse-input input)]
    (->> seeds
         (mapv mapping)
         (apply min))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 35
  (part-1 task-input)                                       ; => 324724204
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
