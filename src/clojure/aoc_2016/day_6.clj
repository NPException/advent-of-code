(ns aoc-2016.day-6
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 6: Signals and Noise ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-6.txt"))

(def test-input "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar")


(defn decode
  [input sort-key]
  (->> (str/split-lines input)
       (u/transpose)
       (map (u/rcomp frequencies #(sort-by sort-key %) first key))
       (apply str)))


(defn part-1
  [input]
  (decode input (comp - val)))


(defn part-2
  [input]
  (decode input val))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => "easter"
  (part-1 task-input)                                       ; => "qzedlxso"
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => "advent"
  (part-2 task-input)                                       ; => "ucmifjae"
  (crit/quick-bench (part-2 task-input))

  )
