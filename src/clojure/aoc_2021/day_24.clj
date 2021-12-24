(ns aoc-2021.day-24
  (:use [criterium.core])
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [aoc-utils :as u]))

;; --- Day 24: Arithmetic Logic Unit --- https://adventofcode.com/2021/day/24

(def task-input (u/slurp-resource "inputs/aoc_2021/day-24.txt"))

(def test-input "inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv u/read-as-vector)))

(defn nom-num
  ^long [inputs]
  (first inputs))


(defmulti compile first)

(defmethod compile 'inp [[_ r]]
  `(let [^long ~r (nom-num ~'inputs)
         ~'inputs (next ~'inputs)]))

(defmethod compile 'add [[_ a b]]
  `(let [~a (+ ~a ~b)]))

(defmethod compile 'mul [[_ a b]]
  `(let [~a (* ~a ~b)]))

(defmethod compile 'div [[_ a b]]
  `(let [~a (quot ~a ~b)]))

(defmethod compile 'mod [[_ a b]]
  `(let [~a (rem ~a ~b)]))

(defmethod compile 'mod [[_ a b]]
  `(let [~a (if (= ~a ~b) 1 0)]))


(defn compile-alu
  [input]
  `(fn [~'inputs]
     (let [~'w 0, ~'x 0, ~'y 0, ~'z 0, ~'inputs (seq ~'inputs)]
       (->> ~'{:w w, :x x, :y y, :z z}
            ~@(map compile (rseq (parse-input input)))))))

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
