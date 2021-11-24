(ns aoc-2020.day-12-alt
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 12: Rain Risk --- https://adventofcode.com/2020/day/12

(def task-input (u/slurp-resource "inputs/aoc_2020/day-12.txt"))

(def test-input "F10\nN3\nF7\nR90\nF11")

;; takes ~ 160 µs with the regular task-input
(defn parse-instructions
  [input]
  (->> (string/split-lines input)
       (into [] (map #(vector (first %) (parse-long (subs % 1)))))))


(defn distance-travelled
  [{:keys [N E] :as _ferry}]
  (+ (u/abs N) (u/abs E)))

(defn run-ferry
  [input ferry commands]
  (->> (reduce
         (fn [frry [op n]]
           ((commands op) frry n))
         (transient ferry)
         (parse-instructions input))
       persistent!))


; part 1

(defn rotate
  [angle deg]
  (-> angle (+ deg) (mod 360)))

(def commands-p1
  {\N #(u/update! %1 :N + %2)
   \S #(u/update! %1 :N - %2)
   \E #(u/update! %1 :E + %2)
   \W #(u/update! %1 :E - %2)
   \L #((commands-p1 \R) %1 (- 360 %2))
   \R #(u/update! %1 :heading rotate %2)
   \F #(let [op ({0 \N, 90 \E, 180 \S, 270 \W} (:heading %1))]
         ((commands-p1 op) %1 %2))})

(defn part-1
  [input]
  (distance-travelled
    (run-ferry input
               {:N       0
                :E       0
                :heading 90}
               commands-p1)))


; part 2

(def commands-p2
  {\N #(u/update! %1 :wp-N + %2)
   \S #(u/update! %1 :wp-N - %2)
   \E #(u/update! %1 :wp-E + %2)
   \W #(u/update! %1 :wp-E - %2)
   \L #((commands-p2 \R) %1 (- 360 %2))
   \R (fn [{:keys [wp-N wp-E] :as ferry} deg]
        (-> ferry
            (assoc! :wp-N (case (int deg)
                            90 (- wp-E)
                            180 (- wp-N)
                            270 wp-E))
            (assoc! :wp-E (case (int deg)
                            90 wp-N
                            180 (- wp-E)
                            270 (- wp-N)))))
   \F (fn [{:keys [wp-N wp-E] :as ferry} n]
        (-> ferry
            (u/update! :N + (* n wp-N))
            (u/update! :E + (* n wp-E))))})


(defn part-2
  [input]
  (distance-travelled
    (run-ferry input
               {:N    0
                :E    0
                :wp-N 1
                :wp-E 10}
               commands-p2)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 25
  (part-1 task-input)                                       ; => 562

  ;; Part 2
  (part-2 test-input)                                       ; => 286
  (part-2 task-input)                                       ; => 101860

  )
