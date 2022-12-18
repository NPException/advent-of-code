(ns aoc-2022.day-16
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 16: Proboscidea Volcanium ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-16.txt"))

(def test-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (u/split-parse line nil
                #"Valve " keyword
                #" has flow rate=" parse-long
                #"; tunnels? leads? to valves? " #(mapv keyword (u/read-as-vector %)))))
       (map (fn [[valve rate adjacent-nodes]]
              [valve [rate (set adjacent-nodes)]]))
       (into {})))


(defn path-length
  [nodes from to]
  (dec (count (u/A*-search [from]
                #(= % to)
                #(second (nodes %))
                (fn [_] 0)
                (fn [_ _] 1)))))


(defn build-path-lengths
  "Returns a map of nodes to [flowrate other-nodes], where `other-nodes`
  is a vector of the node keyword, and the length of the path leading to it.
  {:CC [2 ([:HH 5] [:BB 1] [:EE 2] [:DD 1] [:JJ 4])]"
  [valves]
  (let [valves-with-flow (->> valves
                              (filter (fn [[_ [flow]]]
                                        (> flow 0)))
                              (map key)
                              (set))]
    (reduce
      (fn [acc valve]
        (assoc acc valve
          (->> (disj valves-with-flow valve)
               (map #(vector % (path-length valves valve %)))
               (into {}))))
      {}
      (keys valves))))


(defn build-flow-rates
  [valves]
  (->> valves
       (keep (fn [[valve [flow]]]
               (when (> flow 0)
                 [valve flow])))
       (into {})))


(declare find-next-valve)

(defn weight
  "Determines how much effect on released pressure the given node had if we moved to and opened it."
  [path-lengths rates valves-left valve time-left time-taken]
  ; decrement to include the minute necessary to open the valve
  (let [new-time-left (- time-left time-taken)
        valve-rate    (* new-time-left (rates valve))]
    (cond
      (<= valve-rate 0) 0
      (empty? valves-left) valve-rate
      :else (+ valve-rate
               (first (find-next-valve path-lengths rates valve valves-left new-time-left))))))


(defn find-next-valve
  [path-lengths rates valve valves-left time-left]
  (->> valves-left
       (mapv (fn [next-valve]
               ; time taken to walk to and open the valve
               (let [time-taken (-> path-lengths valve next-valve inc)]
                 [(weight
                    path-lengths rates (disj valves-left next-valve)
                    next-valve
                    time-left
                    time-taken)
                  next-valve
                  time-taken])))
       (sort-by first #(compare %2 %1))
       (first)))


(defn part-1
  [input]
  (let [valve-map    (parse-input input)
        path-lengths (build-path-lengths valve-map)
        rates        (build-flow-rates valve-map)]
    (loop [valves-left (set (keys rates))
           time-left 30
           valve :AA
           released 0
           flow-rate 0]
      (if (zero? time-left)
        released
        (let [[_ next-valve ^long time-taken] (find-next-valve path-lengths rates valve valves-left time-left)]
          (if (or (nil? next-valve) (> time-taken time-left))
            (+ released (* time-left flow-rate))
            (recur
              (disj valves-left next-valve)
              (- time-left time-taken)
              next-valve
              (+ released (* time-taken flow-rate))
              (+ flow-rate ^long (rates next-valve)))))))))


; TODO: If I don't find a solution myself, try translating this: https://www.reddit.com/r/adventofcode/comments/zn6k1l/2022_day_16_solutions/j0nzcnu/
(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1651
  (part-1 task-input)                                       ; => 1701
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 1707
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
