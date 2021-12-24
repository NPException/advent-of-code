(ns aoc-2021.day-23
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]))

;; --- Day 23: Amphipod --- https://adventofcode.com/2021/day/23

(def task-input (u/slurp-resource "inputs/aoc_2021/day-23.txt"))
(def test-input "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")

(defmacro nth-in [v is]
  `(-> ~v ~@(map (fn [i] (list `nth i)) is)))

(defn print-state
  [state]
  (let [e #(-> (nth-in state [% 0]) (or \.))]
    (println "#############")
    (println (str \# (e 8) (e 9) \. (e 10) \. (e 11) \. (e 12) \. (e 13) (e 14) \#))
    (println (str "###" (e 0) \# (e 2) \# (e 4) \# (e 6) "###"))
    (println (str "  #" (e 1) \# (e 3) \# (e 5) \# (e 7) "#"))
    (println "  #########  " (e 15))
    (println)))

;####################
;#8 9 10 11 12 13 14#
;####0##2##4##6######
;   #1##3##5##7#
;   ############

(def positions (vec (range 15)))
(def hallways (vec (drop 8 positions)))

;; spot format: [goal {..neighbour-costs}]
(def spots
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
    positions))

(def COST-INDEX 15)


(defn start-state
  [input]
  (->> (str/split-lines input)
       (drop 2) (take 2)
       (map #(map first (re-seq #"A|B|C|D" %)))
       (apply mapcat vector)
       (map-indexed #(vector %2 0 %1))                      ;; amphipod: [goal cost-so-far id]
       (#(concat % (repeat nil)))
       (take 15)
       vec
       (#(conj % 0))))                                      ;; last state index is cost for last step


(defn path-step-count
  [path]
  (->> (partition 2 1 path)
       (map (fn [[from to]]
              ((nth-in spots [from 1]) to)))
       (apply +)))

(defn find-path
  [[from to]]
  (let [path  (vec (u/A*-search from
                     #(= % to)
                     #(keys (nth-in spots [% 1]))
                     (constantly 0)
                     #((nth-in spots [%1 1]) %2)))
        steps (path-step-count path)]
    [[[from to] [path steps]]
     [[to from] [(vec (rseq path)) steps]]]))

(def paths
  "All possible paths for [from to]"
  (->> (u/combinations 2 positions)
       (mapcat find-path)
       (into {})))

(def hallway? (set hallways))

(def goals {\A [1 0], \B [3 2], \C [5 4], \D [7 6]})        ;; goal spots for each amphipod type, ordered by priority

(defn free-path
  [state [steps :as path]]
  (when (every?
          #(nil? (nth-in state [% 0]))
          (subvec steps 1))                                 ;; ignore the start of the path, since that's the current amphipod position
    path))

(defn goal-path
  "Return a valid path from the given position to the desired goal room"
  [state from goal]
  (let [[end door] (goals goal)
        [other-amph-goal] (state end)]
    (when (and (nil? (state door))
               (or (nil? other-amph-goal) (= goal other-amph-goal)))
      (or (free-path state (paths [from end]))
          (free-path state (paths [from door]))))))

(defn hallway-paths
  "Return all valid paths from a room into the hallway"
  [state from]
  (when-not (hallway? from)
    (keep #(free-path state (paths [from %])) hallways)))

(defn done?
  [state pos]
  (let [[goal] (state pos)
        [end door] (goals goal)]
    (or (= pos end)
        (and (= pos door)
             (= goal (nth-in state [end 0]))))))

(def cost-factor {\A 1, \B 10, \C 100, \D 1000})

(defn move-amphipod
  [state pos]
  (when-not (done? state pos)
    (let [[goal cost-so-far id] (state pos)
          goal-path (goal-path state pos goal)]
      (some->> (seq (cond-> (hallway-paths state pos)
                      goal-path (conj goal-path)))
        (map (fn [path]
               (let [[steps cost] path
                     target    (peek steps)
                     path-cost (* cost (cost-factor goal))]
                 (-> (assoc state pos nil)
                     (assoc target [goal (+ cost-so-far path-cost) id])
                     (assoc COST-INDEX path-cost)))))))))

(defn advance-state
  [state]
  (->> positions
       (into [] (comp
                  (filter #(state %))
                  (keep #(move-amphipod state %))
                  cat))))

(defn finished?
  [state]
  (every?
    #(= (nth-in state [% 0]) (nth-in spots [% 0]))
    [0 1 2 3 4 5 6 7]))

(defn sum-energy
  [state]
  (->> (take 8 state)
       (map second)
       (apply +)))

(defn estimate-finish-cost
  [state]
  (->> positions
       (keep #(when-let [[goal] (nth state %)]
               [% goal]))
       (reduce
         (fn [acc [pos goal]]
           (let [[end door] (goals goal)]
             (if (or (= pos end) (= pos door))
               acc
               (+ acc (* (nth (paths [pos door]) 1)
                        (cost-factor goal))))))
         0)))


(defn part-1
  [input]
  (->> (u/A*-search (start-state input)
         finished?
         advance-state
         estimate-finish-cost
         #_(fn [_] 0)
         #(nth %2 COST-INDEX))
       last
       sum-energy))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 12521
  (part-1 task-input)                                       ; => 19167
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (quick-bench (part-2 task-input))

  )
