(ns aoc-2021.day-24
  (:use [criterium.core])
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 24: Arithmetic Logic Unit --- https://adventofcode.com/2021/day/24

(def task-input (u/slurp-resource "inputs/aoc_2021/day-24.txt"))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv u/read-as-vector)))

(defn nom-num
  ^long [inputs]
  (first inputs))

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


;; NOTE: I keep the ALU around even though we didn't actually need it.
(def alu (eval (compile-alu (parse-input task-input))))


;cond
;	d = 0 -> x&y [1..9]
;	d < 0 ->
;		x = [10-maxy..9]
;		y = [1..9+d]
; d > 0 ->
; 	x = [1..9-d]
; 	y = [10-maxx..9]

(defn ranges
  [^long d]
  (cond
    (zero? d) [[1 9] [1 9]]
    (neg? d) [[(- 10 (+ 9 d)) 9]
              [1 (+ 9 d)]]
    :else [[1 (- 9 d)]
           [(- 10 (- 9 d)) 9]]))

(defn process-block
  [[stack answer] [i [a ^long b c]]]
  (if (= a 1)
    [(conj stack [i c]) answer]
    (let [[partner ^long c] (peek stack)
          [r1 r2] (ranges (+ c b))]
      [(pop stack)
       (-> (assoc answer partner r1)
           (assoc i r2))])))


(defn min-max
  []
  (->> (partition 18 (parse-input task-input))
       (map (juxt #(nth % 4) #(nth % 5) #(nth % 15)))
       (map #(mapv last %))
       (map-indexed vector)
       (reduce process-block [[] {}])
       second
       (#(map % (range 14)))
       ((juxt
          #(str/join (mapv first %))
          #(str/join (mapv second %))))))


(comment
  ;; Part 1
  (last (min-max))                                          ; => 99911993949684
  (quick-bench (min-max))

  ;; Part 2
  (first (min-max))                                         ; => 62911941716111

  )
