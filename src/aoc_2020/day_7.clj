(ns aoc-2020.day-7
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 7: Handy Haversacks --- https://adventofcode.com/2020/day/7

(def task-input
  (u/slurp-resource "inputs/aoc_2020/day-7.txt"))


(defn parse-sub-rule
  [sub-rule]
  (some->> sub-rule
           (re-matches #"(\d+) (.+?) bags?")
           next
           (apply (fn [n color]
                    [color (u/parse-int n)]))))


(defn parse-rule
  [rule-string]
  (let [rule-string (subs rule-string 0 (dec (count rule-string)))
        [color contained] (string/split rule-string #" bags contain ")
        sub-bags (->> (string/split contained #", ")
                      (map parse-sub-rule)
                      (into {}))]
    [color sub-bags]))


(defn parse-rules
  [input]
  (->> (string/split-lines input)
       (map parse-rule)
       (into {})))


(defn invert-rules
  "Returns a map of each color, mapped to a set of all bag colors in which it could be contained"
  [rules]
  (reduce
    (fn [inverted [color contained]]
      (if (empty? contained)
        inverted
        (apply assoc inverted
               (flatten
                 (for [[sub-color] contained]
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
                  (map (fn [[sub-color n]]
                         (+ n (* n (count-bags-inside rules sub-color)))))))))


(comment
  ;; Part 1
  (-> task-input
      parse-rules
      invert-rules
      (bags-containing "shiny gold")
      count)
  ;; Part 2
  (-> task-input
      parse-rules
      (count-bags-inside "shiny gold"))
  )
