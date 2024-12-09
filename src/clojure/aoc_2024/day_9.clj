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

; memory layout structure
; -----------------------
; memory: [file-id ...] - each file id represents one file block. an id of nil represents free space.

;; TODO: use raw array instead, and do array copy instead of expensive single `assoc` calls.

;; TODO: Animation of the compacting process


(defn memory-str
  [memory]
  (str/join (mapv #(if (= -1 %) \. %) memory)))


(defn parse-input
  [text]
  (->> (mapv str text)
       (mapv parse-long)
       (map-indexed (fn [i num]
                      (repeat num (when (even? i) (quot i 2)))))
       (apply concat)
       (vec)))


(defn compact
  ([memory]
   (compact memory
     (u/index-of nil? memory)
     (u/last-index-of some? memory)))
  ([memory first-free-index last-file-index]
   (if (>= first-free-index last-file-index)
     memory
     (let [memory' (-> memory
                      (assoc first-free-index (nth memory last-file-index))
                      (assoc last-file-index nil))]
       (recur memory'
         (u/index-of nil? first-free-index memory')
         (u/last-index-of some? last-file-index memory'))))))


(defn checksum
  [memory]
  (->> memory
       (map-indexed (fn [i file-id]
                      (* i (or file-id 0))))
       (apply +)))


(defn part-1
  [input]
  (->> (parse-input input)
       (compact)
       (checksum)))


;; PART 2 ;;

(defn calc-file-size
  [memory file-id file-end-index]
  (loop [s 0
         i file-end-index]
    (if (and (>= i 0)
             (= (nth memory i) file-id))
      (recur (inc s) (dec i))
      s)))

(defn find-free-index
  [memory size file-end-index]
  (let [free-block (vec (object-array size))]
    ;; pretty inefficient, but meh 🤷‍♂️
    (->> (u/vpartition size 1 memory)
         (take file-end-index)
         (u/index-of #(= % free-block)))))

(defn move-file
  [memory file-id file-size file-end-index free-index]
  (loop [memory' (transient memory)
         copied 0
         file-i file-end-index
         free-i free-index]
    (if (= copied file-size)
      (persistent! memory')
      (recur
        (-> (assoc! memory' file-i nil)
            (assoc! free-i file-id))
        (inc copied)
        (dec file-i)
        (inc free-i)))))

(defn compact-2
  ([memory]
   (compact-2 memory (peek memory) (count memory)))
  ([memory file-id prev-file-index]
   (if (< file-id 0)
     memory
     (let [file-end-index (u/last-index-of #(= % file-id) prev-file-index memory)
           file-size (calc-file-size memory file-id file-end-index)
           free-index (find-free-index memory file-size file-end-index)
           memory' (if free-index
                     (move-file memory file-id file-size file-end-index free-index)
                     memory)]
       (recur memory' (dec file-id) file-end-index)))))


(defn part-2
  [input]
  (->> (parse-input input)
       (compact-2)
       (checksum)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1928
  (part-1 task-input)                                       ; => 6415184586041
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 2858
  (part-2 task-input)                                       ; => 6436819084274 -- takes ~30s to compute tho ...
  (crit/quick-bench (part-2 task-input))

  )
