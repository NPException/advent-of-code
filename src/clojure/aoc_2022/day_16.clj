(ns aoc-2022.day-16
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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


(defn prepare-nodes
  [nodes]
  (let [nodes-with-flow (->> nodes
                             (filter (fn [[_ [flow]]]
                                       (> flow 0)))
                             (map first)
                             (set))]
    (reduce
      (fn [acc [node [rate _]]]
        (assoc acc node [rate (->> (disj nodes-with-flow node)
                                   (map #(vector % (path-length nodes node %))))]))
      {}
      nodes)))


(defn advance-state
  [state]
  (let [{:keys [current nodes open] :as state}
        (-> state
            (update :released u/+l (:total-rate state))
            (update :time-left dec))
        [^long rate adjacent-nodes] (nodes current)]
    (cond->
      ; construct vector of all neighbour states based on where we can move
      (mapv #(assoc state :current %) adjacent-nodes)
      ; if current valve can be opened, add it to the vector of neighbour states
      (not (or (open current) (zero? rate)))
      (conj (-> (update state :total-rate u/+l rate)
                (assoc :open (conj open current)))))))


; TODO: - use `prepare-nodes` to know the only useful paths around
;       - adjust `advance-state` to use the new data and skip multiple minutes instead of single stepping


(defn part-1
  [input]
  (let [nodes       (parse-input input)
        max-rate    (->> (map first (vals nodes))
                         (apply +))
        max-release (* 30 ^long max-rate)
        state       {:nodes      nodes
                     :open       #{}
                     :current    :AA
                     :time-left  30
                     :released   0
                     :total-rate 0}]
    (:released
      (last
        (u/A*-search [state]
          ; goal
          #(zero? ^long (:time-left %))
          ; neighbours
          advance-state
          ; heuristic
          (fn [_] 0.0)
          ; cost
          (fn [current next]
            (- max-release ^long (:released next))))))))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 1651 (15.2 seconds)
  (part-1 task-input)                                       ; =>
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>
  (crit/quick-bench (part-2 task-input))

  )
