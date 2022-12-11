(ns aoc-2016.day-10
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 10: Balance Bots ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-10.txt"))

(def test-input "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2")
;value 5 goes to bot 2
;bot 2 gives low to bot 1 and high to bot 0
;value 3 goes to bot 1
;bot 1 gives low to output 1 and high to bot 0
;bot 0 gives low to output 2 and high to output 0
;value 2 goes to bot 2


(defn parse-line
  "Returns vector of [source-type source-value (low-)target-type (low-)target-value ?high-target-type ?high-target-value]"
  [line]
  (if (str/includes? line "goes to")
    (u/split-parse line
      keyword #" " parse-long
      #" goes to " keyword #" " parse-long)
    (u/split-parse line
      keyword #" " parse-long
      #" gives low to " keyword #" " parse-long
      #" and high to " keyword #" " parse-long)))


(defn generate-target-map
  [input]
  (->> (str/split-lines input)
       (map parse-line)
       (reduce
         (fn [acc [_src-type _src tgt-type tgt high-tgt-type high-tgt :as line]]
           (cond-> (update acc [tgt-type tgt] conj line)
             high-tgt (update [high-tgt-type high-tgt] conj line)))
         {})))


(comment
  {:output {0 17
            1 12}
   :bot    {0 [12 17]}})

(defn determine-value
  [others type id [src-type src low-type low _ _ :as _source]]
  (if (= src-type :value)
    src
    (let [slot (if (and (= low-type type) (= low id)) 0 1)]
      (nth @(get-in others [src-type src]) slot))))


(defn build-node
  [all [type id] [source-a source-b]]
  (if (= type :output)
    ; outputs should only have a single source
    (determine-value all type id source-a)
    ; bots necessarily have 2 sources
    (let [^long a (determine-value all type id source-a)
          ^long b (determine-value all type id source-b)]
      (if (< a b) [a b] [b a]))))


(defn build-resolve-map
  [target-map]
  (let [all (promise)]
    @(deliver all
       (->> target-map
            (reduce-kv
              (fn [acc target sources]
                ;; each node will calculate it's value by deref'ing it's source in the `all` map.
                (assoc-in acc target (delay (build-node @all target sources))))
              {})))))


(defn part-1
  [input desired-values]
  (->> (generate-target-map input)
       (build-resolve-map)
       :bot
       (u/first-match
         (fn [[_id values]]
           (let [[a b] @values]
             (and (desired-values a)
                  (desired-values b)))))
       (first)))


(defn part-2
  [input]
  (let [output (->> (generate-target-map input)
                    (build-resolve-map)
                    :output)]
    (* @(output 0)
       @(output 1)
       @(output 2))))


(comment

  ;; Part 1
  (part-1 test-input #{5 2})                                ; => 2
  (part-1 task-input #{61 17})                              ; => 73
  (crit/quick-bench (part-1 task-input #{61 17}))


  ;; Part 2
  (part-2 test-input)                                       ; => 30
  (part-2 task-input)                                       ; => 3965
  (crit/quick-bench (part-2 task-input))

  )
