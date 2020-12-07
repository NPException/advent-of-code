(ns aoc-2020.day-7
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 7: Handy Haversacks --- https://adventofcode.com/2020/day/7

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-7.txt"))


(defn parse-rule
  [rule-string]
  (let [rule (->> (re-seq #"(?:(.+?) bags contain)|(\d+) (.+?) bag" rule-string)
                  (mapcat next)
                  (remove nil?)
                  (map #(or (u/parse-int %) %)))]
    [(first rule)
     (partition 2 (rest rule))]))


(defn parse-rules
  [input]
  (->> (string/split-lines input)
       (map parse-rule)
       (into {})))


;; remove this stupid thing. Use Jezza's approach instead.
(defn invert-rules
  "Returns a map of each color, mapped to a set of all bag colors in which it could be contained"
  [rules]
  (reduce
    (fn [inverted [color contained-bags]]
      (if (empty? contained-bags)
        inverted
        (apply assoc inverted
               (flatten
                 (for [[_ sub-color] contained-bags]
                   [sub-color (conj (get inverted sub-color #{}) color)])))))
    {}
    rules))


(defn bags-containing
  [inverted-rules color]
  (let [containing (or (inverted-rules color) #{})]
    (into containing
          (mapcat #(bags-containing inverted-rules %))
          containing)))


(defn count-bags-inside
  [rules color]
  (let [contained (rules color)]
    (apply + (->> contained
                  (map (fn [[n sub-color]]
                         (+ n (* n (count-bags-inside rules sub-color)))))))))


(comment
  ;; Part 1 = 213
  (-> task-input
      parse-rules
      invert-rules
      (bags-containing "shiny gold")
      count)
  ;; Part 2 = 38426
  (-> task-input
      parse-rules
      (count-bags-inside "shiny gold"))
  )
