(ns aoc-2021.day-24
  (:use [criterium.core])
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
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

(defn digits
  [n]
  (mapv #(- (int %) 48) (str n)))


(defmulti compile first)

(defmethod compile 'inp [[_ r]]
  `[^long ~r (nom-num ~'inputs)
    ~'inputs (next ~'inputs)])

(defmethod compile 'add [[_ a b]]
  (when-not (= b 0)
    `[~a (+ ~a ~b)]))

(defmethod compile 'mul [[_ a b]]
  (cond
    (= b 0) `[~a 0]
    (not= b 1) `[~a (* ~a ~b)]))

(defmethod compile 'div [[_ a b]]
  (when-not (= b 1)
    `[~a (quot ~a ~b)]))

(defmethod compile 'mod [[_ a b]]
  `[~a (rem ~a ~b)])

(defmethod compile 'eql [[_ a b]]
  `[~a (if (= ~a ~b) 1 0)])


(defn compile-alu
  [instructions]
  `(fn [~'inputs]
     (let [~'w 0, ~'x 0, ~'y 0, ~'z 0, ~'inputs (seq ~'inputs)
           ~@(mapcat compile instructions)]
       ~'{:w w, :x x, :y y, :z z})))


(def alu (eval (compile-alu (parse-input task-input))))
(def test-alu (eval (compile-alu (parse-input test-input))))


(defn part-1
  []
  )


(defn part-2
  []
  )


(comment
  ;; Part 1
  (test-alu [13])                                           ; => {:w 1, :x 1, :y 0, :z 1}
  (alu (digits 13579246899999))                        ; => {:w 9, :x 0, :y 0, :z 87602628}
  (part-1)                                                  ; =>
  (quick-bench (part-1))

  ;; Part 2
  (part-2 test-alu)                                         ; =>
  (part-2)                                                  ; =>
  (quick-bench (part-2))

  )
