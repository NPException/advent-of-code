(ns aoc-2022.day-11
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 11: Monkey in the Middle ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-11.txt"))

(def test-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")


(defn parse-monkey
  [text]
  (let [[_ items operation test if-true if-false] (str/split text #"\n")]
    {:items       (u/read-as-vector (second (str/split items #"items:")))
     :inspections 0
     :inspect-fn  (u/split-parse operation nil #"new = old " str #" " parse-long)
     :aim-fn      (let [[n] (u/split-parse test nil #"by " parse-long)
                        [t] (u/split-parse if-true nil #"monkey " parse-long)
                        [f] (u/split-parse if-false nil #"monkey " parse-long)]
                    [n t f])}))


(defn activate-monkey
  [monkey]
  (-> monkey
      (update :inspect-fn
        (fn [[op x]]
          (case [op (some? x)]
            ["+" true] #(+ % x)
            ["+" false] #(+ % %)
            ["*" true] #(* % x)
            ["*" false] #(* % %))))
      (update :aim-fn
        (fn [[n t f]]
          #(if (= 0 (rem % n)) t f)))))


(defn parse-input
  [input]
  (let [monkeys (->> (str/split input #"\n\n")
                     (mapv parse-monkey))
        magic   (->> monkeys
                     (mapv :aim-fn)
                     (mapv first)
                     (apply *))]
    [magic (mapv activate-monkey monkeys)]))


(defn item-inspection
  [relax-fn inspect-fn aim-fn monkeys item]
  (let [item' (-> (inspect-fn item)
                  (relax-fn))]
    (update-in monkeys [(aim-fn item') :items] conj item')))


(defn turn
  [relax-fn monkeys monkey-index]
  (let [{:keys [items inspect-fn aim-fn]} (nth monkeys monkey-index)]
    (reduce
      #(item-inspection relax-fn inspect-fn aim-fn %1 %2)
      ;; we can already clear the monkey's inventory and increase the inspections counter
      (update monkeys monkey-index
        #(-> (assoc % :items [])
             (update :inspections + (count items))))
      items)))


(defn round
  [relax-fn monkeys]
  (reduce #(turn relax-fn %1 %2)
    monkeys
    (range (count monkeys))))


(defn compute-monkey-business
  [monkeys ^long rounds relax-fn]
  (->> monkeys
       (iterate #(round relax-fn %))
       (take (inc rounds))
       (last)
       (map :inspections)
       (sort #(compare %2 %1))
       (take 2)
       (apply *)))


(defn part-1
  [input]
  (-> (second (parse-input input))
      (compute-monkey-business 20 #(quot % 3))))


(defn part-2
  [input]
  (let [[magic monkeys] (parse-input input)]
    (compute-monkey-business monkeys 10000 #(rem % magic))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10605
  (part-1 task-input)                                       ; => 110264
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 2713310158
  (part-2 task-input)                                       ; => 23612457316
  (crit/quick-bench (part-2 task-input))                    ; ~445 ms

  )
