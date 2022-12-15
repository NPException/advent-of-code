(ns aoc-2020.day-16
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 16: Ticket Translation --- https://adventofcode.com/2020/day/16

;; day 16

(def task-input (u/slurp-resource "inputs/aoc_2020/day-16.txt"))

(def test-input "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")


(defn parse-rule
  [rule-string]
  (let [[key & nums] (string/split rule-string #": |-| or ")
        [a b c d] (map parse-long nums)]
    [key #(or (<= a % b) (<= c % d))]))

(defn parse-rules
  [rules-input]
  (->> (string/trim rules-input)
       string/split-lines
       (map parse-rule)
       (into {})))

(defn parse-tickets
  [tickets-input]
  (->> (string/trim tickets-input)
       string/split-lines
       (map #(string/split % #","))
       (mapv #(mapv parse-long %))))

(defn parse-input
  [input]
  (let [[rules remaining-input] (string/split input #"your ticket:")
        [my-ticket nearby-tickets] (string/split remaining-input #"nearby tickets:")]
    {:rules          (parse-rules rules)
     :my-ticket      (first (parse-tickets my-ticket))
     :nearby-tickets (parse-tickets nearby-tickets)}))


;; part 1

(defn ticket-error
  [matches-no-rule? ticket]
  (->> ticket
       (filter matches-no-rule?)
       (apply +)))

(defn part-1
  [input]
  (let [{:keys [rules nearby-tickets]} (parse-input input)
        matches-no-rule? (complement (apply u/or-fn (vals rules)))]
    (->> nearby-tickets
         (map #(ticket-error matches-no-rule? %))
         (apply +))))


;; part 2

(defn solved?
  [entry]
  (= 1 (count (val entry))))

(defn shake
  [indices]
  (loop [indices indices]
    (let [grouped (group-by solved? indices)
          known-indices (->> (get grouped true)
                             (map (comp first val))
                             (into #{}))
          unsolved (->> (get grouped false)
                        (map key))]
      (if (empty? unsolved)
        indices
        (recur
          (reduce
            #(update %1 %2 set/difference known-indices)
            indices
            unsolved))))))

(defn sieve-ticket
  [name pred indices i v]
  (if (not (pred v))
    (update indices name disj i)
    indices))

(defn sieve-tickets [tickets indices name pred]
  (loop [[ticket & remaining-tickets] tickets
         indices indices]
    (if (nil? ticket)
      indices
      (recur remaining-tickets
             (reduce-kv
               (partial sieve-ticket name pred)
               indices
               ticket)))))

(defn create-index-sets
  [rules]
  (let [indices (set (range (count rules)))]
    (zipmap (keys rules) (repeat indices))))

(defn determine-field-order
  "Returns a sequence of field names as they appear on a ticket."
  [rules tickets]
  (->> rules
       (reduce-kv
         (partial sieve-tickets tickets)
         (create-index-sets rules))
       shake
       (sort-by (comp first val))
       (map key)))

(defn part-2
  [input]
  (let [{:keys [rules my-ticket nearby-tickets]} (parse-input input)
        matches-some-rule? (apply u/or-fn (vals rules))
        valid-tickets (filter #(every? matches-some-rule? %) nearby-tickets)
        fields (determine-field-order rules valid-tickets)]
    (->> fields
         (keep-indexed #(when (string/starts-with? %2 "departure")
                          (my-ticket %1)))
         (apply *))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 71
  (part-1 task-input)                                       ; => 21071

  ;; Part 2
  (part-2 task-input)                                       ; => 3429967441937

  )
