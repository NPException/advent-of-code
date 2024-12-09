(ns aoc-2024.day-9
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 9: Disk Fragmenter ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-9.txt"))

(def test-input "2333133121414131402")

; memory layout structure ideas
; -----------------------
; Idea 1:
; memory: [memory-cell ...] - memory cells represent a range of file blocks, and are ordered by their start index
; memory-cell: [memory-start-index file-id] - (file-id is nil, when the block range is free space)
;
; Idea 2: - current implementation
; memory: [file-id ...] - each file id represents one file block. an id of -1 represents free space.


(defn memory-str
  [memory]
  (str/join (mapv #(if (= -1 %) \. %) memory)))


(defn parse-input
  [text]
  (->> (mapv str text)
       (mapv parse-long)
       (map-indexed (fn [i num]
                      (let [id (if (odd? i) -1 (quot i 2))]
                        (repeat num id))))
       (apply concat)
       (vec)))


(defn compact
  ([memory]
   (compact memory
     (u/index-of #(= % -1) memory)
     (u/last-index-of #(>= % 0) memory)))
  ([memory first-free-index last-file-index]
   (if (>= first-free-index last-file-index)
     memory
     (let [memory' (-> memory
                      (assoc first-free-index (nth memory last-file-index))
                      (assoc last-file-index -1))]
       (recur memory'
         (u/index-of #(= % -1) first-free-index memory')
         (u/last-index-of #(>= % 0) last-file-index memory'))))))


(defn checksum
  [memory]
  (->> memory
       (take-while #(>= % 0))
       (map-indexed (fn [i file-id] (* i file-id)))
       (apply +)))


(defn part-1
  [input]
  (->> (parse-input input)
       (compact)
       (checksum)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1928
  (part-1 task-input)                                       ; => 6415184586041
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
