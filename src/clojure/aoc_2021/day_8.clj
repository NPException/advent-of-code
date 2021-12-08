(ns aoc-2021.day-8
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 8: Seven Segment Search --- https://adventofcode.com/2021/day/8

(def task-input (u/slurp-resource "inputs/aoc_2021/day-8.txt"))

(def small-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(def test-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn parse-line
  [line]
  (let [[inputs outputs] (str/split line #" \| ")]
    [inputs (str/split outputs #" ")]))


(defn count-unique-output-digits
  [line]
  (->> (parse-line line)
       (second)
       (filter #(#{2 4 3 7} (count %)))
       count))

(defn part-1
  [input]
  (->> (str/split-lines input)
       (map count-unique-output-digits)
       (apply +)))

;; Calculating the frequencies of each segment in all digits,
;; and then adding the frequency for each segment within each digit
;; gives a unique sum for each digit. These are hardcoded here.
(def known-frequency-sums
  {42 0, 17 1, 34 2, 39 3, 30 4, 37 5, 41 6, 25 7, 49 8, 45 9})

(defn solve-line
  [[inputs outputs]]
  (let [freqs (frequencies inputs)]
    (->> outputs
         (mapv #(->> (mapv freqs %) (apply +) known-frequency-sums))
         (str/join)
         (parse-long))))

(defn part-2
  [input]
  (->> (str/split-lines input)
       (pmap #(-> % parse-line solve-line))
       (apply +)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 26
  (part-1 task-input)                                       ; => 495
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 small-input)                                      ; => 5353
  (part-2 test-input)                                       ; => 61229
  (part-2 task-input)                                       ; => 1055164
  (quick-bench (part-2 task-input))

  )
