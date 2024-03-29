(ns aoc-2020.day-12
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
  (+ (abs N) (abs E)))

(defn run-ferry
  [input ferry execute-fn]
  (->> (reduce
         execute-fn
         (transient ferry)
         (parse-instructions input))
       persistent!))


; part 1

(defmulti execute-command (fn [_ferry [op _n]] op))

(defmethod execute-command \N [ferry [_ n]]
  (u/update! ferry :N + n))

(defmethod execute-command \S [ferry [_ n]]
  (u/update! ferry :N - n))

(defmethod execute-command \E [ferry [_ n]]
  (u/update! ferry :E + n))

(defmethod execute-command \W [ferry [_ n]]
  (u/update! ferry :E - n))

(defmethod execute-command \L [ferry [_ deg]]
  (execute-command ferry [\R (- 360 deg)]))

(defmethod execute-command \R [ferry [_ deg]]
  (u/update! ferry :heading #(-> % (+ deg) (mod 360))))

(defmethod execute-command \F [ferry [_ n]]
  (let [op ({0 \N, 90 \E, 180 \S, 270 \W} (:heading ferry))]
    (execute-command ferry [op n])))


(defn part-1
  [input]
  (distance-travelled
    (run-ferry input
               {:N       0
                :E       0
                :heading 90}
               execute-command)))


; part 2

(defmulti execute-waypoint-command (fn [_ferry [op _n]] op))

(defmethod execute-waypoint-command \N [ferry [_ n]]
  (u/update! ferry :wp-N + n))

(defmethod execute-waypoint-command \S [ferry [_ n]]
  (u/update! ferry :wp-N - n))

(defmethod execute-waypoint-command \E [ferry [_ n]]
  (u/update! ferry :wp-E + n))

(defmethod execute-waypoint-command \W [ferry [_ n]]
  (u/update! ferry :wp-E - n))

(defmethod execute-waypoint-command \L [ferry [_ deg]]
  (execute-waypoint-command ferry [\R (- 360 deg)]))

(defmethod execute-waypoint-command \R [{:keys [wp-N wp-E] :as ferry} [_ deg]]
  (-> ferry
      (assoc! :wp-N (case (int deg)
                      90 (- wp-E)
                      180 (- wp-N)
                      270 wp-E))
      (assoc! :wp-E (case (int deg)
                      90 wp-N
                      180 (- wp-E)
                      270 (- wp-N)))))

(defmethod execute-waypoint-command \F [{:keys [wp-N wp-E] :as ferry} [_ n]]
  (-> ferry
      (u/update! :N + (* n wp-N))
      (u/update! :E + (* n wp-E))))


(defn part-2
  [input]
  (distance-travelled
    (run-ferry input
               {:N    0
                :E    0
                :wp-N 1
                :wp-E 10}
               execute-waypoint-command)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 25
  (part-1 task-input)                                       ; => 562

  ;; Part 2
  (part-2 test-input)                                       ; => 286
  (part-2 task-input)                                       ; => 101860

  )
