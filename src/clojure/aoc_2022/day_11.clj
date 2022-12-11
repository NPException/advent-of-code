(ns aoc-2022.day-11
  (:require [aoc-utils :as u]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 11: Monkey in the Middle ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-11.txt"))

(def test-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")


;Monkey 0:
;  Starting items: 79, 98
;  Operation: new = old * 19
;  Test: divisible by 23
;    If true: throw to monkey 2
;    If false: throw to monkey 3

(defn build-fn
  [op a b]
  )


(defn parse-monkey
  [text]
  (let [[_ items operation test if-true if-false]
        (str/split text #"\n")]
    {:items       (u/read-as-vector (second (str/split items #"items:")))
     :inspections 0
     :inspect     (let [[op x] (u/split-parse operation nil #"new = old " str #" " #(or (parse-long %) %))
                        f (case op "+" + "*" *)]
                    (if (string? x)
                      #(f % %)
                      #(f % x)))
     :aim         (let [[n] (u/split-parse test nil #"by " parse-long)
                        [t] (u/split-parse if-true nil #"monkey " parse-long)
                        [f] (u/split-parse if-false nil #"monkey " parse-long)]
                    [n t f]
                    #(if (= 0 (mod % n)) t f))}))


(defn parse-input
  [input]
  (->> (str/split input #"\n\n")
       (mapv parse-monkey)))


(defn item-inspection
  [inspect-fn aim-fn monkeys item]
  (let [item'         (inspect-fn item)
        item''        (quot item' 3)                        ;; relief / monkey gets bored
        target-index  (aim-fn item'')
        target-monkey (nth monkeys target-index)]
    (assoc monkeys target-index
      (update target-monkey :items conj item''))))


(defn turn
  [monkeys monkey-index]
  (let [{:keys [items inspect aim] :as monkey} (nth monkeys monkey-index)]
    (reduce
      #(item-inspection inspect aim %1 %2)
      (assoc monkeys monkey-index
        (-> (assoc monkey :items [])
            (update :inspections + (count items))))
      items)))


(defn round
  [monkeys]
  (reduce turn monkeys (range (count monkeys))))


(defn part-1
  [input]
  (->> (parse-input input)
       (iterate round)
       (take 21)
       (last)
       (map :inspections)
       (sort #(compare %2 %1))
       (take 2)
       (apply *)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10605
  (part-1 task-input)                                       ; => 110264
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
