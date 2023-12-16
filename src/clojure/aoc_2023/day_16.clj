(ns aoc-2023.day-16
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 16: The Floor Will Be Lava ---

(def task-input (u/slurp-resource "inputs/aoc_2023/day-16.txt"))

(def test-input ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....")

(defn move
  "Move a beam position by a direction offset.
  Returns a vector of the new position and the direction."
  [[x y] [ox oy :as dir]]
  [[(+ x ox) (+ y oy)] dir])


(defmulti beam-step
  "Returns a vector of possible follow up [beam position, beam direction]"
  {:arglists '([tile beam])}
  (fn [tile _beam]
    tile))

; If the beam encounters empty space (.), it continues in the same direction.
(defmethod beam-step \.
  [_ [pos dir]]
  [(move pos dir)])

; If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending
; on the angle of the mirror. For instance, a rightward-moving beam that encounters
; a / mirror would continue upward in the mirror's column, while a rightward-moving beam
; that encounters a \ mirror would continue downward from the mirror's column.
(defmethod beam-step \\
  [_ [pos [ox oy]]]
  [(move pos [oy ox])])

(defmethod beam-step \/
  [_ [pos [ox oy]]]
  [(move pos [(- oy) (- ox)])])

; If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter
; as if the splitter were empty space. For instance, a rightward-moving beam that
; encounters a - splitter would continue in the same direction.
; If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams
; going in each of the two directions the splitter's pointy ends are pointing. For instance,
; a rightward-moving beam that encounters a | splitter would split into two beams:
; one that continues upward from the splitter's column and one that continues downward
; from the splitter's column.

(defmethod beam-step \|
  [_ [pos [ox _oy :as dir]]]
  (if (zero? ox)
    [(move pos dir)]
    [(move pos [0 -1])
     (move pos [0 1])]))

(defmethod beam-step \-
  [_ [pos [_ox oy :as dir]]]
  (if (zero? oy)
    [(move pos dir)]
    [(move pos [-1 0])
     (move pos [1 0])]))


(defn tile-at
  [grid [[x y] _dir :as _beam]]
  (u/nth-in grid [y x] nil))


(defn count-energized
  [grid start-beam]
  (loop [energized #{}
         [beam & unprocessed] [start-beam]
         seen      #{}]
    (cond
      (nil? beam) (count energized)
      (seen beam) (recur energized unprocessed seen)
      :else (if-let [tile (tile-at grid beam)]
              (recur
                (conj energized (first beam))
                (into unprocessed (beam-step tile beam))
                (conj seen beam))
              ; out of bounds
              (recur energized unprocessed (conj seen beam))))))


(defn part-1
  [input]
  (count-energized
    (str/split-lines input)
    [[0 0] [1 0]]))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 46
  (part-1 task-input)                                       ; => 6855
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
