(ns aoc-2024.day-4
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 4: Ceres Search ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-4.txt"))

(def test-input "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX")


; offsets for the words XMAS and SAMX
(def offsets
  [[[0 0] [1 0] [2 0] [3 0]]                                ; horizontal
   [[0 0] [0 1] [0 2] [0 3]]                                ; vertical
   [[0 0] [1 1] [2 2] [3 3]]                                ; diagonal down
   [[0 3] [1 2] [2 1] [3 0]]])                              ; diagonal up


(defmacro coordinates
  [x y offsets]
  (mapv (fn [[ox oy]]
          [(if (zero? ox) x `(+ ~x ~ox))
           (if (zero? oy) y `(+ ~y ~oy))])
    offsets))


(defn xmas? [grid [[ax ay] [bx by] [cx cy] [dx dy]]]
  (let [a (u/nth-in grid [ay ax])
        b (u/nth-in grid [by bx])
        c (u/nth-in grid [cy cx])
        d (u/nth-in grid [dy dx])]
    (or (and (= a \X) (= b \M) (= c \A) (= d \S))
        (and (= a \S) (= b \A) (= c \M) (= d \X)))))


(defn part-1
  [input]
  (let [grid (str/split-lines input)
        width (count (first grid))
        max-x (- width 4)
        height (count grid)
        max-y (- height 4)]
    (->> (for [x (range 0 width)
               y (range 0 height)
               :let [horizontal? (<= x max-x)
                     vertical? (<= y max-y)
                     diagonal? (and horizontal? vertical?)]]
           [(if horizontal? (coordinates x y [[0 0] [1 0] [2 0] [3 0]])) ; horizontal
            (if vertical? (coordinates x y [[0 0] [0 1] [0 2] [0 3]])) ; vertical
            (if diagonal? (coordinates x y [[0 0] [1 1] [2 2] [3 3]])) ; diagonal down
            (if diagonal? (coordinates x y [[0 3] [1 2] [2 1] [3 0]]))])
         (apply concat)
         (filter some?)
         (u/count-matching #(xmas? grid %))))) ; diagonal up


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 18
  (part-1 task-input)                                       ; => 2560
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
