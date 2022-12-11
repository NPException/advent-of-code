(ns aoc-2016.day-9
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [criterium.core :as crit])
  (:import (java.util.regex Matcher)))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 9: Explosives in Cyberspace ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-9.txt"))

(def test-input "X(8x2)(3x3)ABCY")


(defn expand
  [text from]
  (let [matcher (-> (re-matcher #"^\((\d+)x(\d+)\)" text)
                    (.region from (count text)))
        [marker block-size repetitions] (re-find matcher)
        block-size (parse-long block-size)
        repetitions (parse-long repetitions)
        marker-size (count marker)
        offset (+ marker-size block-size)
        block (subs text (+ from marker-size) (+ from offset))]
    [offset
     (.repeat block repetitions)]))


(defn decompress
  [text]
  (let [size (count text)]
    (loop [r []
           s 0
           e 0]
      (if (= e size)
        (str/join (conj r (subs text s e)))
        (if (= \( (.charAt text e))
          ; append current section to `r `, then append decompressed following section, then continue after
          (let [r' (conj r (subs text s e))
                [offset expansion] (expand text e)
                e' (+ e offset)]
            (recur (conj r' expansion) e' e'))
          (recur r s (inc e)))))))


(defn part-1
  [input]
  (->> (decompress input)
       (u/count-matching #(not (Character/isWhitespace ^char %)))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 18
  (part-1 task-input)                                       ; =>
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
