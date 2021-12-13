(ns aoc-2021.day-13
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 13: Transparent Origami --- https://adventofcode.com/2021/day/13

(def task-input (u/slurp-resource "inputs/aoc_2021/day-13.txt"))

(def test-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")

(defn parse-input
  [input]
  (let [[points folds] (str/split input #"\n\n")]
    [(->> (str/split points #"\n")
          (mapv #(read-string (str "[" % "]")))
          (set))
     (->> (str/split folds #"\n")
          (map #(concat (rest (re-find #"(x|y)=(\d+)" %))))
          (mapv (fn [[xy n]] [(if (= xy "x") 0 1) (parse-long n)])))]))


(defn fold
  [points [axis i]]
  (into #{} (comp (remove #(= i (nth % axis)))
                  (map (fn [point]
                         (let [val (nth point axis)]
                           (if (> (nth point axis) i)
                             (assoc point axis (- i (- val i)))
                             point)))))
        points))


(defn part-1
  [input]
  (let [[points folds] (parse-input input)]
    (count (fold points (first folds)))))


(defn part-2
  [input]
  (let [result (apply reduce fold (parse-input input))
        max-x (apply max (map #(nth % 0) result))
        max-y (apply max (map #(nth % 1) result))]
    (doseq [y (range (inc max-y))
            x (range (inc max-x))]
      (if (result [x y])
        (print "#")
        (print " "))
      (when (= x max-x)
        (println)))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 17
  (part-1 task-input)                                       ; => 827
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 16
  (part-2 task-input)                                       ;
  ; ####  ##  #  # #  # ###  ####  ##  ###
  ; #    #  # #  # # #  #  # #    #  # #  #
  ; ###  #  # #### ##   #  # ###  #    #  #
  ; #    #### #  # # #  ###  #    #    ###
  ; #    #  # #  # # #  # #  #    #  # #
  ; #### #  # #  # #  # #  # ####  ##  #
  (quick-bench (part-2 task-input))

  )
