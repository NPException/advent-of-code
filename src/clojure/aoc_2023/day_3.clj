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
     :digits  (into {} digits)
     :gears   (into {}
                (filter #(= \* (second %)))
                symbols)}))

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

(defn build-numbers
  [digits digit-elements]
  (->> digit-elements
       (into []
         (comp
           (distinct)
           (map #(build-number digits %))
           (distinct)))))


(def offsets
  (vec (for [dy (range -1 2)
             dx (range -1 2)]
         [dx dy])))

(defn find-neighbours
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
  (let [{:keys [symbols digits]} (parse-input input)]
    (->> symbols
         (reduce-kv
           (fn [neighbours xy _sym]
             (into neighbours (find-neighbours digits xy)))
           [])
         (build-numbers digits)
         (map second)
         (apply +))))


(defn part-2
  [input]
  (let [{:keys [gears digits]} (parse-input input)]
    (->> gears
         (map #(->> (first %)
                    (find-neighbours digits)
                    (build-numbers digits)
                    (mapv second)))
         (filter #(= 2 (count %)))
         (map #(apply * %))
         (apply +))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 4361
  (part-1 task-input)                                       ; => 540212
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 467835
  (part-2 task-input)                                       ; => 87605697
  (crit/quick-bench (part-2 task-input))

  )
