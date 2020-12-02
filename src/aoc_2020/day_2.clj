(ns aoc-2020.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; --- Day 2: Password Philosophy ---

(def task-input
  (-> (io/resource "inputs/aoc-2020/day-2.txt")
      slurp))


(defn line->rule-set
  [line]
  (when-let [[_ n1 n2 c password] (re-matches #"(\d+)-(\d+) (.): (.+)" line)]
    [(Integer/parseInt n1)
     (Integer/parseInt n2)
     (first c)
     password]))


(defn count-valid-passwords
  [input valid-password?]
  (->> input
       string/split-lines
       (map line->rule-set)
       (filter #(apply valid-password? %))
       count))


(defn matching-char-count?
  [n1 n2 c password]
  (<= n1
      (get (frequencies password) c 0)
      n2))


(comment
  ;; Part 1
  (count-valid-passwords task-input matching-char-count?)
  )
