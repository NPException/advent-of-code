(ns aoc-2021.day-10
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 10: Syntax Scoring --- https://adventofcode.com/2021/day/10

(def task-input (u/slurp-resource "inputs/aoc_2021/day-10.txt"))

(def test-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")

(def illegal-score {\) 3, \] 57, \} 1197, \> 25137})
(def partner {\( \), \[ \], \{ \}, \< \>})

(defn analyse
  [input calculate-score]
  (->> (str/split-lines input)
       (pmap #(reduce
               (fn [stack c]
                 (if-let [closing (partner c)]
                   (conj stack closing)
                   (if (= c (peek stack))
                     (pop stack)
                     (reduced (illegal-score c)))))
               [] %))
       calculate-score))

(defn part-1
  [input]
  (analyse input #(->> % (filterv number?) (apply +))))


(defn completion-score
  [stack]
  (->> (mapv {\) 1, \] 2, \} 3, \> 4} (rseq stack))
       (reduce #(+ (* %1 5) %2))))

(defn median
  [values]
  (nth (sort values) (quot (count values) 2)))

(defn part-2
  [input]
  (analyse input #(->> % (remove number?) (mapv completion-score) median)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 26397
  (part-1 task-input)                                       ; => 358737
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 288957
  (part-2 task-input)                                       ; => 4329504793
  (quick-bench (part-2 task-input))

  )
