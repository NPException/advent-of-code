(ns aoc-2022.day-5
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 5: Supply Stacks ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-5.txt"))

(def test-input "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")


(defn parse-cargo
  [input]
  (->> (str/split-lines input)
       (drop-last)
       (mapv (fn [line]
               (->> (rest line)
                    (take-nth 4)
                    (mapv #(if-not (= \space %) %)))))
       (u/rows->columns)
       (mapv #(vec (rseq (filterv some? %))))))


(defn parse-moves
  [input]
  (->> (str/split-lines input)
       (mapv #(u/split-parse %
                nil #"move " parse-long
                #" from " (comp dec parse-long)
                #" to " (comp dec parse-long)))))


(defn parse-input
  [input]
  (let [[cargo moves] (str/split input #"\n\n")]
    [(parse-cargo cargo)
     (parse-moves moves)]))


(defn go-go-crane
  [input reverse?]
  (let [[cargo moves] (parse-input input)]
    (->> (reduce
           (fn [stacks [n from to]]
             (let [from-stack (nth stacks from)
                   keep-n     (- (count from-stack) n)
                   to-stack   (nth stacks to)
                   transform  (if reverse? rseq identity)]
               (-> stacks
                   (assoc from (subvec from-stack 0 keep-n))
                   (assoc to (apply conj to-stack (transform (subvec from-stack keep-n)))))))
           cargo moves)
         (map peek)
         (apply str))))


(defn part-1
  [input]
  (go-go-crane input true))


(defn part-2
  [input]
  (go-go-crane input false))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => "CMZ"
  (part-1 task-input)                                       ; => "MQTPGLLDN"
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => "MCD"
  (part-2 task-input)                                       ; => "LVZPSTTCZ"
  (crit/quick-bench (part-2 task-input))

  )
