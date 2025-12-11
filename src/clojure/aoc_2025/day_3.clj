(ns aoc-2025.day-3
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 3: Lobby ---

(def task-input (u/slurp-resource "inputs/aoc_2025/day-3.txt"))

(def test-input "987654321111111\n811111111111119\n234234234234278\n818181911112111")

(defn line->digits
  [line]
  (mapv #(- (int %) 48) line))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv line->digits)))

; biggest values and lowest index is most important
(defn importance
  [[i x]]
  (-> (* x 10000)
      (- i)))


(defn jottage
  [battery-line]
  (let [max-i (dec (count battery-line))
        indexed (->> battery-line
                     (map-indexed vector)
                     (vec))
        [i1 x1] (apply max-key importance indexed)]
    (if (= i1 max-i)
      ;; biggest digit was at the end
      (let [[_ x2] (->> (subvec indexed 0 max-i)
                        (apply max-key importance))]
        (+ x1 (* x2 10)))
      ;; find next biggest digit
      (let [[_ x2] (->> (subvec indexed (inc i1) (inc max-i))
                        (apply max-key importance))]
        (+ (* x1 10) x2)))))


(defn part-1
  [input]
  (->> (parse-input input)
       (mapv jottage)
       (apply +)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 357
  (part-1 task-input)                                       ; => 17427
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
