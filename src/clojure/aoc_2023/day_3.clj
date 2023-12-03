(ns aoc-2023.day-3
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 3: Gear Ratios ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-3.txt"))

(def test-input (str/join "\n" ["467..114.."
                                "...*......"
                                "..35..633."
                                "......#..."
                                "617*......"
                                ".....+.58."
                                "..592....."
                                "......755."
                                "...$.*...."
                                ".664.598.."]))


(defn parse-input
  [input]
  (let [grid (str/split-lines input)
        {digits  true
         symbols false} (->> (u/grid-elements grid)
                             (keep (fn [[x y ^char e]]
                                     (when (not= e \.)
                                       [[x y] e])))
                             (group-by (fn [[_xy ^char e]]
                                         (Character/isDigit e))))]
    {:symbols (into {} symbols)
     :digits  (into {} digits)}))

(defn build-number
  [digits [[x y] _digit]]
  (let [start-offset (or (->> (range)
                              (take-while #(digits [(- x %) y]))
                              (last))
                         0)
        end-offset   (or (->> (range)
                              (take-while #(digits [(+ x %) y]))
                              (last))
                         0)
        start-x      (- x start-offset)
        number       (->> (range start-x (inc (+ x end-offset)))
                          (map #(digits [% y]))
                          (apply str)
                          (parse-long))]
    [[start-x y] number]))

(defn find-numbers
  [input neighbour-fn]
  (let [{:keys [symbols digits]} (parse-input input)]
    (->> symbols
         (reduce-kv
           (fn [neighbours xy _sym]
             (into neighbours (neighbour-fn digits xy)))
           [])
         (into []
           (comp
             (distinct)
             (map #(build-number digits %))
             (distinct)
             (map second))))))


(def offsets
  (vec (for [dy (range -1 2)
             dx (range -1 2)]
         [dx dy])))

(defn neighbours
  [digit-map [x y]]
  (reduce
    (fn [acc [dx dy]]
      (if-let [entry (find digit-map [(+ x dx) (+ y dy)])]
        (conj acc entry)
        acc))
    []
    offsets))


(defn part-1
  [input]
  (apply + (find-numbers input neighbours)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 4361
  (part-1 task-input)                                       ; =>
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
