(ns aoc-2023.day-1
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 1: Trebuchet?! ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-1.txt"))

(def test-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")
(def test-input-2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")


(defn solve [input parse]
  (->> (str/split-lines input)
       (map parse)
       (apply +)))


(defn parse-digits
  [s]
  (let [digits (filterv #(Character/isDigit (char %)) s)]
    (parse-long (str (first digits) (peek digits)))))

(defn part-1
  [input]
  (solve input parse-digits))

;;

(def word-digits {"one" "1", "two" "2", "three" "3", "four" "4", "five" "5", "six" "6", "seven" "7", "eight" "8", "nine" "9"})

(def forward-regex #"\d|one|two|three|four|five|six|seven|eight|nine")
(def reverse-regex #"enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|\d")

(defn parse-digits-and-words
  [s]
  (let [low (re-find forward-regex s)
        high (str/reverse (re-find reverse-regex (str/reverse s)))]
    (parse-long (str (word-digits low low) (word-digits high high)))))

(defn part-2
  [input]
  (solve input parse-digits-and-words))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 142
  (part-1 task-input)                                       ; => 54667
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input-2)                                     ; => 281
  (part-2 task-input)                                       ; => 54203
  (crit/quick-bench (part-2 task-input))

  )
