(ns aoc-2016.day-7
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 7: Internet Protocol Version 7 ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-7.txt"))

(def test-input "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn")


(defn abba?
  [[a b c d]]
  (and (not= a b)
       (= a d)
       (= b c)))

(defn contains-abba?
  [section]
  (->> (name section)
       (partition 4 1)
       (some abba?)))


(defn part-1
  [input]
  (->> (str/split-lines input)
       (map u/read-as-vector)
       (u/count-matching
         (fn [sections]
           (and (->> sections (filter symbol?) (some contains-abba?))
                (->> sections (filter vector?) (map first) (not-any? contains-abba?)))))))



(defn ssl?
  [sections]
  (let [needed-bab? (->> (filter symbol? sections)
                         (map name)
                         (mapcat (fn [section]
                                   (->> (partition 3 1 section)
                                        (keep (fn [[a b c]]
                                                (when (and (not= a b) (= a c))
                                                  (str b a b)))))))
                         (into #{}))]
    (->> (filter vector? sections)
         (map first)
         (map name)
         (mapcat (fn [section]
                   (->> (partition 3 1 section)
                        (mapv #(apply str %)))))
         (some needed-bab?))))


(defn part-2
  [input]
  (->> (str/split-lines input)
       (map u/read-as-vector)
       (u/count-matching ssl?)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 2
  (part-1 task-input)                                       ; => 115
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 task-input)                                       ; => 231
  (crit/quick-bench (part-2 task-input))

  )
