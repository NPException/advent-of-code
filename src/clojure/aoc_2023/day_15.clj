(ns aoc-2023.day-15
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 15: Lens Library ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-15.txt"))

(def test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")


; Algorithm:
;   Determine the ASCII code for the current character of the string.
;   Increase the current value by the ASCII code you just determined.
;   Set the current value to itself multiplied by 17.
;   Set the current value to the remainder of dividing itself by 256.
(defn HASH
  [s]
  (reduce
    (fn [h c]
      (-> (+ h (int c))
          (* 17)
          (mod 256)))
    0
    s))


(defn part-1
  [input]
  (->> (str/split input #",")
       (mapv HASH)
       (apply +)))


;; PART 2

(defn remove-lens
  [box [label _focal-length]]
  (if-let [i (u/index-of #(= label (first %)) box)]
    (vec (concat (subvec box 0 i) (subvec box (inc i))))
    box))

(defn replace-lens
  [box [label _focal-length :as lens]]
  (if-let [i (u/index-of #(= label (first %)) box)]
    (assoc box i lens)
    (conj box lens)))

(defn HASHMAP
  [boxes op]
  (let [[label focal-length :as lens] (str/split op #"[=-]")]
    (update boxes (HASH label)
      (if focal-length
        replace-lens
        remove-lens)
      lens)))


(defn focusing-power
  [box-index box]
  (->> box
       (map-indexed (fn [lens-index [_label focal-length]]
                      (* (inc box-index)
                         (inc lens-index)
                         (parse-long focal-length))))
       (apply +)))


(defn part-2
  [input]
  (let [ops (str/split input #",")
        boxes (vec (repeat 256 []))]
    (->> (reduce HASHMAP boxes ops)
         (map-indexed focusing-power)
         (apply +))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1320
  (part-1 task-input)                                       ; => 514639
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 145
  (part-2 task-input)                                       ; => 279470
  (crit/quick-bench (part-2 task-input))

  )
