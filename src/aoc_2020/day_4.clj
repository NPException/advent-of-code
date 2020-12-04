(ns aoc-2020.day-4
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 4: Passport Processing --- https://adventofcode.com/2020/day/4

(def task-input
  (u/slurp-resource "inputs/aoc-2020/day-4.txt"))


;; common parts

(defn parse-passport
  [passport-string]
  (->> (string/split passport-string #"\s")
       (map #(string/split % #":"))
       (into {})))

(defn count-valid-passports
  [input validator]
  (->> (string/split input #"\n\n")
       (map parse-passport)
       (filter validator)
       count))


;; validator for part 1

(defn all-required-fields?
  [passport]
  (every? passport ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))


;; validator for part 2

(def field-rules
  {"byr" #(when-let [year (u/parse-int %)]
            (<= 1920 year 2002))
   "iyr" #(when-let [year (u/parse-int %)]
            (<= 2010 year 2020))
   "eyr" #(when-let [year (u/parse-int %)]
            (<= 2020 year 2030))
   "hgt" #(when-let [[_ height unit] (re-matches #"(\d{2,3})(in|cm)" %)]
            (if (= unit "cm")
              (<= 150 (u/parse-int height) 193)
              (<= 59 (u/parse-int height) 76)))
   "hcl" #(re-matches #"#[0-9a-f]{6}" %)
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" #(re-matches #"\d{9}" %)})

(defn field-data-valid?
  [passport]
  (every?
    (fn [[field rule]]
      (some-> (passport field) rule))
    field-rules))


(comment
  ;; Part 1
  (count-valid-passports task-input all-required-fields?)
  ;; Part 2
  (count-valid-passports task-input field-data-valid?)
  )
