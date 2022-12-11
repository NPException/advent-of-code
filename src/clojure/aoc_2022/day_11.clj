(ns aoc-2022.day-11
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 11: Monkey in the Middle ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-11.txt"))

(def test-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")


(def &items 0)
(def &inspections 1)
(def &inspect-fn 2)
(def &aim-fn 3)


(defn parse-monkey
  [text]
  (let [[_ items operation test if-true if-false]
        (str/split text #"\n")]
    (transient
      (-> []
          (assoc &items
            (transient (u/read-as-vector (second (str/split items #"items:")))))
          (assoc &inspections
            0)
          (assoc &inspect-fn
            (let [[op ^long x] (u/split-parse operation nil #"new = old " str #" " parse-long)]
              (case [op (some? x)]
                ["+" true] (fn [^long item] (+ item x))
                ["+" false] (fn [^long item] (+ item item))
                ["*" true] (fn [^long item] (* item x))
                ["*" false] (fn [^long item] (* item item)))))
          (assoc &aim-fn
            (let [[^long n] (u/split-parse test nil #"by " parse-long)
                  [t] (u/split-parse if-true nil #"monkey " parse-long)
                  [f] (u/split-parse if-false nil #"monkey " parse-long)]
              #(if (= 0 (rem ^long % n)) t f)))))))


(defn parse-input
  [input]
  (->> (str/split input #"\n\n")
       (mapv parse-monkey)))


(defn item-inspection
  [relief-fn inspect-fn aim-fn monkeys item]
  (let [item'         (inspect-fn item)
        item''        (relief-fn item')                     ;; relief / monkey gets bored
        target-index  (aim-fn item'')
        target-monkey (nth monkeys target-index)]
    (assoc! monkeys target-index
      (assoc! target-monkey
        &items
        (conj! (nth target-monkey &items) item'')))))


(defn update-inspections!
  [monkey ^long n]
  (let [acc (+ n ^long (nth monkey &inspections))]
    (-> (assoc! monkey &items (transient []))
        (assoc! &inspections acc))))


(defn turn
  [relief-fn monkeys monkey-index]
  (let [monkey  (nth monkeys monkey-index)
        items   (nth monkey &items)
        inspect (nth monkey &inspect-fn)
        aim     (nth monkey &aim-fn)]
    (reduce
      #(item-inspection relief-fn inspect aim %1 %2)
      (assoc! monkeys monkey-index
        (update-inspections! monkey (count items)))
      (persistent! items))))


(defn round
  [relief-fn monkeys]
  (reduce #(turn relief-fn %1 %2)
    monkeys
    (range (count monkeys))))


(defn compute-monkey-business
  [input ^long rounds relief-fn]
  (->> (parse-input input)
       (transient)
       (iterate #(round relief-fn %))
       (take (inc rounds))
       (last)
       (persistent!)
       (map #(nth % &inspections))
       (sort #(compare %2 %1))
       (take 2)
       (apply *)))


(defn part-1
  [input]
  (compute-monkey-business input 20 #(quot ^long % 3)))


;; TODO: somehow keep the numbers within a confined range, without changing divisibilities
(defn part-2
  [input]
  (compute-monkey-business input 10000 identity))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10605
  (part-1 task-input)                                       ; => 110264
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 2713310158
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
