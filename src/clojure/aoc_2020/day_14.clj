(ns aoc-2020.day-14
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 14:  --- https://adventofcode.com/2020/day/14

(def task-input (u/slurp-resource "inputs/aoc_2020/day-14.txt"))

(def test-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0")


(defn parse-command
  [[op arg-1 arg-2]]
  (case op
    "mask" [:mask
            (-> arg-2 (string/replace \X \0) (u/parse-long 2))
            (-> arg-2 (string/replace \X \1) (u/parse-long 2))]
    "mem" [:mem
           (u/parse-long arg-1)
           (u/parse-long arg-2)]))


(defn parse-program
  [input]
  (->> (string/split-lines input)
       (map (comp rest #(re-matches #"(mask|mem)\[?(\d*?)\]? = (\w+)" %)))
       (mapv parse-command)))


(defn find-address-length
  [program]
  (->> program
       (filter (comp #{:mem} first))
       (map second)
       (apply max)
       inc))


(defmulti step-program-1 (fn [_ [op _ _]] op))

(defmethod step-program-1 :mask
  [vm [_ mask-1 mask-0]]
  (-> vm
      (assoc :mask-1 mask-1)
      (assoc :mask-0 mask-0)))

(defmethod step-program-1 :mem
  [{:keys [mask-1 mask-0] :as vm} [_ address value]]
  (update vm :memory
          assoc
          address
          (-> value
              (bit-or mask-1)
              (bit-and mask-0))))


(defn part-1
  [input]
  (let [program (parse-program input)
        vm {:mask-1 0
            :mask-0 0
            :memory (-> (find-address-length program)
                        (repeat 0)
                        vec)}]
    (->> program
         (reduce step-program-1 vm)
         :memory
         (apply +))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 165
  (part-1 task-input)                                       ; => 15172047086292

  ;; Part 2
  (part-2 test-input)
  (part-2 task-input)

  )
