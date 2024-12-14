(ns aoc-2024.day-14
  (:require [aoc-utils :as u]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 14: Restroom Redoubt ---

(def task-input (u/slurp-resource "inputs/aoc_2024/day-14.txt"))

(def test-input "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3")

(defn parse-robots
  [input]
  (->> (str/replace input \= \,)
       (u/read-as-vector)
       (u/vpartition 6)
       (mapv (fn [[_ x y _ vx vy]]
               [x y vx vy]))))


(defn print-robots
  [robots width height]
  (let [robots-map (->> (map #(vec (take 2 %)) robots)
                        (frequencies))]
    (doseq [y (range 0 height)]
      (doseq [x (range 0 width)]
        (print (robots-map [x y] \space)))
      (println))))


(defn simulate
  [robots seconds width height]
  (->> robots
       (mapv (fn [[x y vx vy]]
               [(-> (* vx seconds) (+ x) (mod width))
                (-> (* vy seconds) (+ y) (mod height))
                vx vy]))))


(defn count-robots-within
  [robots [min-x max-x min-y max-y]]
  (->> robots
       (u/count-matching
         (fn [[x y]]
           (and (< min-x x max-x)
                (< min-y y max-y))))))

(defn safety-factor
  [robots width height]
  (let [mid-x (quot width 2)
        mid-y (quot height 2)]
    (->> [[-1 mid-x -1 mid-y]
          [mid-x width -1 mid-y]
          [-1 mid-x mid-y height]
          [mid-x width mid-y height]]
         (mapv #(count-robots-within robots %))
         (apply *))))

(defn part-1
  [input width height]
  (-> (parse-robots input)
      (simulate 100 width height)
      (safety-factor width height)))


(comment
  ;; Part 1
  (part-1 test-input 11 7)                                  ; => 12
  (part-1 task-input 101 103)                               ; => 221655456

  ;; Part 2 => 7858

  ; explanation: visually skimmed through the first couple hundred prints,
  ; and noticed the following:
  ; - at 2707 every 101 seconds some horizontal bunching
  ; - at 2708 every 103 seconds some vertical bunching

  ; to find the first positive number in the 101 cycle: 81
  ; to find the first positive number in the 103 cycle: 30

  ; final step: build sets of the repeating steps and pray that the first shared number is not too big
  (->> (set/intersection
         (set (take 10000 (iterate #(+ % 101) 81)))
         (set (take 10000 (iterate #(+ % 103) 30))))
       (apply min))
  ; => 7858

  (-> (parse-robots task-input)
      (simulate 7858 101 103)
      (print-robots 101 103))

  )
