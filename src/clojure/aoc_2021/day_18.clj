(ns aoc-2021.day-18
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [aoc-utils :as u]))

;; --- Day 18: Snailfish --- https://adventofcode.com/2021/day/18

(def task-input (u/read-edn-lines (u/slurp-resource "inputs/aoc_2021/day-18.txt")))

(def test-input (u/read-edn-lines "[1,2]\n[[1,2],3]\n[9,[8,7]]\n[[1,9],[8,5]]\n[[[[1,2],[3,4]],[[5,6],[7,8]]],9]\n[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]\n[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"))


(defn find-explode-path
  [sn level path]
  (when-let [[a b] (and (vector? sn) sn)]
    (or (and (= level 4) path)
        (find-explode-path a (inc level) (conj path 0))
        (find-explode-path b (inc level) (conj path 1)))))


(defn pop-while
  [path i]
  (loop [path path]
    (if (= i (peek path))
      (recur (pop path))
      path)))

(defn branch-to-number
  [sn path i]
  (loop [path path]
    (when-let [node (get-in sn path)]
      (if (number? node)
        path
        (recur (conj path i))))))

(defn find-number
  [sn path side]
  (let [path (pop-while path side)]
    (when (seq path)
      (branch-to-number
        sn
        (-> (pop path) (conj side))
        (bit-xor side 1)))))

(defn explode
  "Returns the exploded snail number, if there was something to explode."
  ([sn]
   (when-let [path (find-explode-path sn 0 [])]
     (explode sn path)))
  ([sn path]
   (let [[a b] (get-in sn path)
         left-path (find-number sn path 0)
         right-path (find-number sn path 1)]
     (cond-> (assoc-in sn path 0)
       left-path (update-in left-path + a)
       right-path (update-in right-path + b)))))


(defn part-1
  [input]
  )


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; =>
  (part-1 task-input)                                       ; =>
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (quick-bench (part-2 task-input))

  )
