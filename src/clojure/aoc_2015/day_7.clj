(ns aoc-2015.day-7
  (:require [aoc-utils :as u]))

;; --- Day 7: Some Assembly Required --- https://adventofcode.com/2015/day/7

(def task-input (u/slurp-resource "inputs/aoc_2015/day-7.txt"))

(def test-input "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i")


(defn parse-element
  [x]
  (or (parse-long x) (keyword x)))

(defn parse-input
  [input]
  (->> input
       (re-seq #"(?:([a-z0-9]+) )?(?:([A-Z]+) )?(?:(\w+) )-> ([a-z]+)")
       (map rest)
       (map #(mapv parse-element %))))


(defn trim-16-bit
  [x]
  (bit-and x 0xFFFF))

(defn unwrap
  [wires x]
  (when x
    (if (number? x)
      x
      @(wires x))))

(defn build-command
  [promised-wires [a op b dest]]
  [dest
   (delay
     (let [wires @promised-wires
           a (unwrap wires a)
           b (unwrap wires b)]
       (trim-16-bit
         (case op
           nil b
           :NOT (bit-not b)
           :AND (bit-and a b)
           :OR (bit-or a b)
           :LSHIFT (bit-shift-left a b)
           :RSHIFT (bit-shift-right a b)))))])


(defn build-wire-map
  [input]
  (let [promised-wires (promise)
        wires (->> (parse-input input)
                   (map #(build-command promised-wires %))
                   (into {}))]
    (deliver promised-wires wires)
    wires))


(defn part-1
  [input wire]
  @(wire (build-wire-map input)))


(defn part-2
  [input part-1-solution]
  @(:a (build-wire-map (str input "\n" part-1-solution " -> b"))))


(comment
  ;; Part 1
  (part-1 test-input :h)                                    ; => 65412
  (part-1 task-input :a)                                    ; => 3176

  ;; Part 2
  (part-2 task-input (part-1 task-input :a))                ; => 14710

  )
