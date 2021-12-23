(ns aoc-2021.day-23
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 23: Amphipod --- https://adventofcode.com/2021/day/23

(def task-input (u/slurp-resource "inputs/aoc_2021/day-23.txt"))
(def test-input "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")

(def costs {\A 1, \B 10, \C 100, \D 1000})

;####################
;#8 9 10 11 12 13 14#
;####0##2##4##6######
;   #1##3##5##7#
;   ############

;; spot format: [occupant goal {..neighbour-costs}]
(def spots
  (mapv
    {0  [nil \A {1 1, 9 2, 10 2}]
     1  [nil \A {0 1}]
     2  [nil \B {3 1, 10 2, 11 2}]
     3  [nil \B {2 1}]
     4  [nil \C {5 1, 11 2, 12 2}]
     5  [nil \C {4 1}]
     6  [nil \D {7 1, 12 2, 13 2}]
     7  [nil \D {6 1}]
     8  [nil nil {9 1}]
     9  [nil nil {8 1, 0 2, 10 2}]
     10 [nil nil {9 2, 0 2, 2 2, 11 2}]
     11 [nil nil {10 2, 2 2, 4 2, 12 2}]
     12 [nil nil {11 2, 4 2, 6 2, 13 2}]
     13 [nil nil {12 2, 6 2, 14 1}]
     14 [nil nil {13 1}]}
    (range 15)))

(defn start-state
  [input]
  (->> (str/split-lines input)
       (drop 2) (take 2)
       (map #(map first (re-seq #"A|B|C|D" %)))
       (apply mapcat vector)
       (map-indexed vector)
       (reduce
         (fn [acc [i v]]
           (assoc-in acc [i 0] [v 0]))
         spots)))


(defn print-state
  [state]
  (let [e (fn [i] (-> i state first first (or \.)))]
    (println "#############")
    (println (str \# (e 8) (e 9) \. (e 10) \. (e 11) \. (e 12) \. (e 13) (e 14) \#))
    (println (str "###" (e 0) \# (e 2) \# (e 4) \# (e 6) "###"))
    (println (str "  #" (e 1) \# (e 3) \# (e 5) \# (e 7) "#"))
    (println "  #########")))

(defmacro nth-in [v is]
  `(-> ~v ~@(map (fn [i] (list `nth i)) is)))

(defn empty-neighbour?
  [state neighbours]
  (not (every?
         (fn [[i _]] (nth-in state [i 0]))
         neighbours)))

(defn path-to-room
  [state i]
  ; TODO find path from hallway position i to goal-room
  )

(defn can-move?
  [state i]
  (let [[[amph cost] room neighbours] (state i)]
    (and (or (zero? cost) (not= amph room))
         (empty-neighbour? state neighbours)
         (or room (path-to-room state i)))))

(defn move-amphipod
  [state i]
  (when (can-move? state i)
    ;; TODO
    ))

(defn advance-state
  [state]
  (->> (range 15)
       (into [] (comp
                  (filter #(first (state %)))
                  (keep #(move-amphipod state %))
                  cat))))

(defn finished?
  [state]
  (every?
    (fn [[[x] goal]] (= x goal))
    (take 8 state)))

#_(defn finished?
    [state]
    (defn finished?
      [state]
      (and (let [[[v] goal] (state 0)] (= v goal))
           (let [[[v] goal] (state 1)] (= v goal))
           (let [[[v] goal] (state 2)] (= v goal))
           (let [[[v] goal] (state 3)] (= v goal))
           (let [[[v] goal] (state 4)] (= v goal))
           (let [[[v] goal] (state 5)] (= v goal))
           (let [[[v] goal] (state 6)] (= v goal))
           (let [[[v] goal] (state 7)] (= v goal)))))

(defn sum-energy
  [state]
  (-> (take 8 state)
      (map #(get-in % [0 1]))
      (apply +)))

(defn part-1
  [input]
  (loop [[state & more-states] [(start-state input)]
         end-states '()]
    (if state
      (let [{done true, more false} (group-by finished? (advance-state state))]
        (recur (concat more-states more) (concat end-states done)))
      (->> end-states
           (map sum-energy)
           (apply min)))))

(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; =>
  (part-1 task-input)                                       ; =>
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (quick-bench (part-2 task-input))

  )
