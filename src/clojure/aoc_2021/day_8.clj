(ns aoc-2021.day-8
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 8: Seven Segment Search --- https://adventofcode.com/2021/day/8

(def task-input (u/slurp-resource "inputs/aoc_2021/day-8.txt"))

(def small-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(def test-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn parse-line
  [line]
  (->> (str/split line #"\|")
       (mapv #(filterv seq (map set (str/split % #"\s+"))))))


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


;; only returns wiresets that have exactly n elements
(defn filter-active-wires
  [n inputs]
  (filterv #(#{n} (count %)) inputs))

(defn build-digits
  [inputs]
  (let [maybe-235 (filter-active-wires 5 inputs)
        maybe-069 (filter-active-wires 6 inputs)
        one (first (filter-active-wires 2 inputs))
        contains-one? #(= one (set/intersection one %))
        three (first (filter contains-one? maybe-235))
        four (first (filter-active-wires 4 inputs))
        contains-four? #(= four (set/intersection four %))
        six (first (remove contains-one? maybe-069))
        contained-in-six? #(= % (set/intersection six %))
        five (first (filter contained-in-six? maybe-235))
        two (first (remove #{three five} maybe-235))
        seven (first (filter-active-wires 3 inputs))
        eight (first (filter-active-wires 7 inputs))
        nine (first (filter contains-four? maybe-069))
        zero (first (remove #{six nine} maybe-069))]
    {zero 0, one 1, two 2, three 3, four 4, five 5, six 6, seven 7, eight 8, nine 9}))

(defn solve-line
  [[inputs outputs]]
  (->> (map (build-digits inputs) outputs)
       (apply str)
       (parse-long)))

(defn part-2
  [input]
  (->> (str/split-lines input)
       (map (comp solve-line parse-line))
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
