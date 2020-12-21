(ns aoc-2020.day-21
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 21:  --- https://adventofcode.com/2020/day/21

(def task-input (u/slurp-resource "inputs/aoc_2020/day-21.txt"))

(def test-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)")


(defn parse-food
  [line]
  (let [data (read-string (str "[" line "]"))]
    {:ingredients (set (butlast data))
     :allergens   (set (rest (last data)))}))

(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map parse-food)))


(defn safe-intersection
  [s1 s2]
  (if s1
    (set/intersection s1 s2)
    s2))

(defn potential-allergens
  "Returns a set of ingredients that might be allergens"
  [foods]
  (->> foods
       (mapcat (fn [{:keys [ingredients allergens]}]
                 (map #(vector % ingredients) allergens)))
       (reduce
         (fn [acc [allergen ingredients]]
           (update acc allergen safe-intersection ingredients))
         {})))

(defn part-1
  [input]
  (let [foods (parse-input input)]
    (->> (mapcat :ingredients foods)
         (filter (complement (->> (potential-allergens foods)
                                  (mapcat val)
                                  (into #{}))))
         count)))


(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 5
  (part-1 task-input)                                       ; => 1945

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )
