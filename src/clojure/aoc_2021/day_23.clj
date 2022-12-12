(ns aoc-2021.day-23
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 23: Amphipod --- https://adventofcode.com/2021/day/23

(def task-input (u/slurp-resource "inputs/aoc_2021/day-23.txt"))
(def test-input "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")

(defn print-state
  [state]
  (let [rooms (range (- (count state) 8))
        n (count rooms)
        depth (/ n 4)
        columns (partition depth rooms)
        e #(-> (u/nth-in state [% 0]) (or \.))]
    (println "#############")
    (println (str \# (e n) (e (+ n 1)) \. (e (+ n 2)) \. (e (+ n 3)) \. (e (+ n 4)) \. (e (+ n 5)) (e (+ n 6)) \#))
    (println (str "###"
               (str/join "#" (->> (map #(nth % 0) columns)
                                  (map #(get-in state [% 0] \.))))
               "###"))
    (dotimes [i (dec ^long depth)]
      (println (str "  #"
                 (str/join "#" (->> (map #(nth % (inc i)) columns)
                                    (map #(get-in state [% 0] \.))))
                 "#")))
    (println "  #########  " (peek state))
    (println)))

;####################
;#8 9 10 11 12 13 14#
;####0##2##4##6######
;   #1##3##5##7#
;   ############

;; spot format: [goal {..neighbour-costs}]
(def spots-1
  (mapv
    {0  [\A {1 1, 9 2, 10 2}]
     1  [\A {0 1}]
     2  [\B {3 1, 10 2, 11 2}]
     3  [\B {2 1}]
     4  [\C {5 1, 11 2, 12 2}]
     5  [\C {4 1}]
     6  [\D {7 1, 12 2, 13 2}]
     7  [\D {6 1}]
     8  [nil {9 1}]
     9  [nil {8 1, 0 2, 10 2}]
     10 [nil {9 2, 0 2, 2 2, 11 2}]
     11 [nil {10 2, 2 2, 4 2, 12 2}]
     12 [nil {11 2, 4 2, 6 2, 13 2}]
     13 [nil {12 2, 6 2, 14 1}]
     14 [nil {13 1}]}
    (range 15)))

;##########################
;#16 17  18  19  20  21 22#
;###### 0## 4## 8##12######
;     + 1++ 5++ 9++13+
;     + 2++ 6++10++14+
;     # 3## 7##11##15#
;     ################

(def spots-2
  (mapv
    {0  [\A {1 1, 17 2, 18 2}]
     1  [\A {2 1, 0 1}]
     2  [\A {3 1, 1 1}]
     3  [\A {2 1}]
     4  [\B {5 1, 18 2, 19 2}]
     5  [\B {6 1, 4 1}]
     6  [\B {7 1, 5 1}]
     7  [\B {6 1}]
     8  [\C {9 1, 19 2, 20 2}]
     9  [\C {10 1, 8 1}]
     10 [\C {11 1, 9 1}]
     11 [\C {10 1}]
     12 [\D {13 1, 20 2, 21 2}]
     13 [\D {14 1, 12 1}]
     14 [\D {15 1, 13 1}]
     15 [\D {14 1}]
     16 [nil {17 1}]
     17 [nil {16 1, 18 2, 0 2}]
     18 [nil {0 2, 17 2, 19 2, 4 2}]
     19 [nil {4 2, 18 2, 20 2, 8 2}]
     20 [nil {8 2, 19 2, 21 2, 12 2}]
     21 [nil {12 2, 20 2, 22 1}]
     22 [nil {21 1}]}
    (range 23)))

(defn start-state
  [input spots lines-transform]
  (->> (str/split-lines input)
       (drop 2) (take 2)
       lines-transform
       (map #(map first (re-seq #"A|B|C|D" %)))
       (apply mapcat vector)
       (map-indexed #(vector %2 0 %1))                      ;; amphipod: [goal cost-so-far id]
       (#(concat % (repeat nil)))
       (take (count spots))
       vec
       (#(conj % 0))))                                      ;; last state index is cost for last step


(defn path-step-count
  [spots path]
  (->> (partition 2 1 path)
       (map (fn [[from to]]
              ((u/nth-in spots [from 1]) to)))
       (apply +)))

(defn find-path
  [spots [from to]]
  (let [path  (vec (u/A*-search [from]
                     #(= % to)
                     #(keys (u/nth-in spots [% 1]))
                     (constantly 0)
                     #((u/nth-in spots [%1 1]) %2)))
        steps (path-step-count spots path)]
    [[[from to] [path steps]]
     [[to from] [(vec (rseq path)) steps]]]))

(defn generate-paths
  "All possible paths for [from to]"
  [spots]
  (->> (u/combinations 2 (range (count spots)))
       (mapcat #(find-path spots %))
       (into {})))

(defn free-path
  [state [steps :as path]]
  "Returns the given path if it is free of obstacles, otherwise returns nil."
  (when (every?
          #(nil? (u/nth-in state [% 0]))
          (subvec steps 1))                                 ;; ignore the start of the path, since that's the current amphipod position
    path))

(defn append-step
  [[path ^long length] pos]
  [(conj path pos) (inc length)])

(defn goal-path
  "Return a valid path from the given position to the desired goal room"
  [paths goals state from goal]
  (let [[door & more] (rseq (goals goal))]
    (when (and (nil? (state door))
               (every? #(let [g (some-> (nth state %) (nth 0))]
                          (or (nil? g) (= g goal)))
                 more))
      (when-let [path (free-path state (paths [from door]))]
        (reduce
          append-step
          path
          (take-while #(nil? (nth state %)) more))))))

(defn hallway-paths
  "Return all valid paths from a room into the hallway, or nil if there are no valid paths"
  [hallways hallway? paths state from]
  (when-not (hallway? from)
    (seq (keep #(free-path state (paths [from %])) hallways))))

(defn done?
  [goals state pos]
  (let [[goal] (state pos)
        [end & more :as room] (goals goal)]
    (or (= pos end)
        (and (some #(= % pos) more)
             (every? #(let [g (some-> (nth state %) (nth 0))]
                        (or (nil? g) (= g goal)))
               room)))))

(defn cost-factor
  ^long [x]
  (case (int x)
    65 1                                                    ;; A
    66 10                                                   ;; B
    67 100                                                  ;; C
    68 1000))                                               ;; D

(defn move-amphipod
  [hallways hallway? paths goals state pos]
  (when-not (done? goals state pos)
    (let [cost-index (dec (count state))
          [goal ^long cost-so-far id] (state pos)
          goal-path  (goal-path paths goals state pos goal)]
      (some->> (if goal-path
                 [goal-path]
                 (hallway-paths hallways hallway? paths state pos))
        (map (fn [path]
               (let [[steps ^long length] path
                     target    (peek steps)
                     path-cost (* length (cost-factor goal))]
                 (-> (assoc state pos nil)
                     (assoc target [goal (+ cost-so-far path-cost) id])
                     (assoc cost-index path-cost)))))))))

(defn advance-state
  [positions hallways hallway? paths goals state]
  (->> positions
       (into [] (comp
                  (filter #(state %))
                  (keep #(move-amphipod hallways hallway? paths goals state %))
                  cat))))

(defn finished?
  [spots rooms state]
  (every?
    #(= (u/nth-in state [% 0]) (u/nth-in spots [% 0]))
    rooms))

(defn sum-energy
  [state]
  (->> (drop-last 8 state)
       (map second)
       (apply +)))

(defn estimate-finish-cost
  [positions paths goals state]
  (->> positions
       (keep #(when-let [[goal] (nth state %)]
                [% goal]))
       (reduce
         (fn [^long acc [pos goal]]
           (let [room (goals goal)
                 door (peek room)]
             (if (or (= pos door) (done? goals state pos))
               acc
               (let [^long path-length (nth (paths [pos door]) 1)]
                 (+ acc (* path-length (cost-factor goal)))))))
         0)))


(defn solve
  [input spots lines-transform]
  ; goals -> {\A [1 0], \B [3 2], \C [5 4], \D [7 6]}  goal spots for each amphipod type, ordered by priority
  (let [positions (vec (range (count spots)))
        hallways  (vec (take-last 7 positions))
        hallway?  (set hallways)
        rooms     (vec (drop-last 7 positions))
        goals     (->> (partition (/ (count rooms) 4) rooms)
                       (map (comp vec reverse))
                       (zipmap [\A \B \C \D]))
        paths     (generate-paths spots)
        start (start-state input spots lines-transform)]
    (->> (u/A*-search [start]
           #(finished? spots rooms %)
           #(advance-state positions hallways hallway? paths goals %)
           #(estimate-finish-cost positions paths goals %)
           #(peek %2))
         last
         sum-energy)))


(defn part-1
  [input]
  (solve input spots-1 identity))


(defn part-2
  [input]
  (solve input spots-2
    (fn [[l1 l2]]
      [l1
       "  #D#C#B#A#"
       "  #D#B#A#C#"
       l2])))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 12521
  (part-1 task-input)                                       ; => 19167
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 44169 -> ~21s
  (part-2 task-input)                                       ; => 47665 -> ~17s
  (quick-bench (part-2 task-input))

  )
