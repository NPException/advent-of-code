(ns aoc-2016.day-9
  (:require [aoc-utils :as u]
            [criterium.core :as crit]))

; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 9: Explosives in Cyberspace ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-9.txt"))

(def test-input "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")


(defn count-non-whitespace
  (^long [text]
   (count-non-whitespace text nil))
  (^long [text _]
   (u/count-matching #(not (Character/isWhitespace ^char %)) text)))


(defn expand
  [text from count-fn]
  (let [matcher           (-> (re-matcher #"^\((\d+)x(\d+)\)" text)
                              (.region from (count text)))
        [marker block-size repetitions] (re-find matcher)
        block-size  (parse-long block-size)
        repetitions (parse-long repetitions)
        marker-size       (count marker)
        offset            (+ marker-size block-size)
        block             (subs text (+ from marker-size) (+ from offset))]
    [offset
     (* (count-fn block count-fn)
        repetitions)]))


(defn decompress
  [^String text count-fn]
  (let [size (count text)]
    (loop [length 0
           start  0
           end    0]
      (if (= end size)
        (+ length (count-non-whitespace (subs text start end)))
        (if (= \( (.charAt text end))
          (let [[^long offset
                 ^long expansion-length] (expand text end count-fn)
                continue-at (+ end offset)]
            (recur
              (+ length
                 (count-non-whitespace (subs text start end))
                 expansion-length)
              continue-at
              continue-at))
          (recur length start (inc end)))))))


(defn part-1
  [input]
  (decompress input count-non-whitespace))


(defn part-2
  [input]
  (decompress input decompress))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 238
  (part-1 task-input)                                       ; => 115118
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 445
  (part-2 task-input)                                       ; => 11107527530
  (crit/quick-bench (part-2 task-input))                    ; ~870 µs

  )
