(ns aoc-2021.day-12
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 12: Passage Pathing --- https://adventofcode.com/2021/day/12

(def task-input (u/slurp-resource "inputs/aoc_2021/day-12.txt"))

(def test-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def test-input-2 "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")
(def test-input-3 "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")

(defn parse-paths
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (mapcat (juxt identity reverse))
       (u/group-by-and-map first second)))


(def only-once? #(Character/isLowerCase (char (first %))))

(defn update-seen
  [seen pos]
  (if (and (only-once? pos) (seen pos))
    (conj seen :visited-twice)
    (conj seen pos)))

(defn valid-next-point?
  [seen pos]
  (or (not (only-once? pos))
      (and (not= "start" pos)
           (or (not (seen pos))
               (not (seen :visited-twice))))))

(defn branch-out
  [navi [ongoing done] [last-pos seen]]
  (if-let [next-points (->> (navi last-pos)
                            (filterv #(valid-next-point? seen %))
                            (not-empty))]
    [(into ongoing
           (comp (remove #(= "end" %))
                 (map #(vector % (update-seen seen %))))
           next-points)
     (+ done (count (filter #(= "end" %) next-points)))]
    [ongoing done]))

(defn solve
  [input allow-double-visit?]
  (let [navi (parse-paths input)]
    (loop [[ongoing done] [[["start" #{"start" (or allow-double-visit? :visited-twice)}]]
                           0]]
      (if (empty? ongoing)
        done
        (recur (reduce #(branch-out navi %1 %2)
                       [[] done]
                       ongoing))))))

(defn part-1
  [input]
  (solve input false))


(defn part-2
  [input]
  (solve input true))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 10
  (part-1 test-input-2)                                     ; => 19
  (part-1 test-input-3)                                     ; => 226
  (part-1 task-input)                                       ; => 3679
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 36
  (part-2 test-input-2)                                     ; => 103
  (part-2 test-input-3)                                     ; => 3509
  (part-2 task-input)                                       ; => 107395
  (quick-bench (part-2 task-input))

  )
