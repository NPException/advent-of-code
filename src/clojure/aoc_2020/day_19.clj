(ns aoc-2020.day-19
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 19:  --- https://adventofcode.com/2020/day/19

(def task-input (u/slurp-resource "inputs/aoc_2020/day-19.txt"))

(def test-input "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb")


(defn parse-rule
  [rules rule-string]
  (let [[id condition] (string/split rule-string #": ")]
    [id
     (delay
       (if (string/starts-with? condition "\"")
         (subs condition 1 (dec (count condition)))
         (str "(?:"
              (-> condition
                  (string/replace #"\d+" #(deref (@rules %)))
                  (string/replace " " ""))
              ")")))]))


(defn parse-rules
  [rules-input]
  (let [rules (promise)]
    (->> (string/split-lines rules-input)
         (map (partial parse-rule rules))
         (into {})
         (deliver rules))
    (reduce-kv #(assoc %1 %2 @%3) {} @rules)))


(defn parse-input
  [input]
  (let [[rules messages] (string/split input #"\n\n")]
    [(parse-rules rules)
     (string/split-lines messages)]))


(defn part-1
  [input]
  (let [[rules messages] (parse-input input)
        rule-0-regex (re-pattern (rules "0"))]
    (->> messages
         (filter #(re-matches rule-0-regex %))
         count)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 2
  (part-1 task-input)                                       ; => 241

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
