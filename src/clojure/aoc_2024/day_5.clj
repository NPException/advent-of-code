(ns aoc-2024.day-5
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 5: Print Queue ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-5.txt"))

(def test-input "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47")

(defn parse-input
  [input]
  (let [[rules updates] (->> (str/replace input \| \,)
                             (str/split-lines)
                             (mapv u/read-as-vector)
                             (split-with seq))]
    ; first entry in `updates` will be an empty vector, so we discard it.
    [(vec rules)
     (vec (drop 1 updates))]))


(defn rules->map
  [rules]
  (reduce
    (fn [acc [low high]]
      (update acc low #(conj (or % #{}) high)))
    {}
    rules))


(defn correct-order?
  [rules-map update-values]
  (->> (u/vpartition 2 1 update-values)
       (every? (fn [[a b]]
                 (contains? (rules-map a) b)))))


(defn part-1
  [input]
  (let [[rules updates] (parse-input input)
        rules-map (rules->map rules)]
    (->> updates
         (filter #(correct-order? rules-map %))             ; remove incorrect updates
         (map #(nth % (quot (count %) 2)))                  ; get middle values
         (apply +))))                                       ; sum


(defn part-2
  [input]
  (let [[rules updates] (parse-input input)
        rules-map (rules->map rules)
        before? (fn [a b]
                  (contains? (rules-map a) b))]
    (->> updates
         (remove #(correct-order? rules-map %))             ; remove correct updates
         (map #(vec (sort before? %)))                      ; re-order incorrect updates
         (map #(nth % (quot (count %) 2)))                  ; get middle values
         (apply +))))                                       ; sum


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 143
  (part-1 task-input)                                       ; => 6034
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 123
  (part-2 task-input)                                       ; => 6305
  (crit/quick-bench (part-2 task-input))

  )
