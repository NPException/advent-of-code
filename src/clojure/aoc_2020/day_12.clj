(ns aoc-2020.day-12
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 12: Rain Risk --- https://adventofcode.com/2020/day/12

(def task-input (u/slurp-resource "inputs/aoc_2020/day-12.txt"))

(def test-input "F10\nN3\nF7\nR90\nF11")


(def ferry {:N       0
            :E       0
            :heading 90})


(defn parse-instructions
  [input]
  (->> (string/split-lines input)
       (into [] (comp
                  (map #(re-matches #"([NSEWLRF])(\d+)" %))
                  (map (fn [[_ op n]]
                         [(keyword op) (u/parse-long n)]))))))


(defmulti execute-command (fn [_ferry [op _n]] op))

(defmethod execute-command :N [ferry [_ n]]
  (u/update! ferry :N + n))

(defmethod execute-command :S [ferry [_ n]]
  (u/update! ferry :N - n))

(defmethod execute-command :E [ferry [_ n]]
  (u/update! ferry :E + n))

(defmethod execute-command :W [ferry [_ n]]
  (u/update! ferry :E - n))

(defn turn
  [heading +- degrees]
  (-> heading
      (+- degrees)
      (mod 360)))

(defmethod execute-command :L [ferry [_ n]]
  (u/update! ferry :heading turn - n))

(defmethod execute-command :R [ferry [_ n]]
  (u/update! ferry :heading turn + n))

(defmethod execute-command :F [ferry [_ n]]
  (let [op ({0 :N 90 :E 180 :S 270 :W} (:heading ferry))]
    (execute-command ferry [op n])))


(defn distance-travelled
  [{:keys [N E] :as _ferry}]
  (+ (u/abs N) (u/abs E)))



(defn part-1
  [input]
  (->> (reduce
         execute-command
         (transient ferry)
         (parse-instructions input))
       persistent!
       distance-travelled))



(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 25
  (part-1 task-input)                                       ; => 562

  ;; Part 2
  (part-2 test-input)
  (part-2 task-input)

  )
