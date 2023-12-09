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

(defn find-num
  ^long [^String s ^long start-index update-index]
  (loop [i start-index]
    (cond
      (or (.startsWith s "1" i) (.startsWith s "one" i)) 1
      (or (.startsWith s "2" i) (.startsWith s "two" i)) 2
      (or (.startsWith s "3" i) (.startsWith s "three" i)) 3
      (or (.startsWith s "4" i) (.startsWith s "four" i)) 4
      (or (.startsWith s "5" i) (.startsWith s "five" i)) 5
      (or (.startsWith s "6" i) (.startsWith s "six" i)) 6
      (or (.startsWith s "7" i) (.startsWith s "seven" i)) 7
      (or (.startsWith s "8" i) (.startsWith s "eight" i)) 8
      (or (.startsWith s "9" i) (.startsWith s "nine" i)) 9
      :else (recur (long (update-index i))))))

(defn find-num-l
  ^long [s]
  (find-num s 0 #(inc ^long %)))

(defn find-num-r
  ^long [s]
  (find-num s (dec (count s)) #(dec ^long %)))

(defn parse-digits-and-words
  [s]
  (+ (* 10 (find-num-l s))
     (find-num-r s)))

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
