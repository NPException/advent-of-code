(ns aoc-2020.day-21
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 21: Allergen Assessment --- https://adventofcode.com/2020/day/21

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
  (if s1 (set/intersection s1 s2) s2))

(defn potential-allergens
  "Returns a map of allergens and the ingredient candidates that might contain them"
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
         (remove (->> (potential-allergens foods)
                      (mapcat val)
                      (into #{})))
         count)))


;; part 2

(defn resolve-allergens
  "Returns a map of which ingredient contains which allergen"
  [foods]
  (let [candidates (potential-allergens foods)]
    (loop [resolved (->> candidates
                         (filter #(= 1 (count (val %))))
                         (map (fn [[k v]]
                                [(first v) k]))
                         (into {}))]
      (if (= (count resolved) (count candidates))
        resolved
        (recur (reduce-kv
                 (fn [res allergen ingredients]
                   (let [unknowns (filter (complement res) ingredients)]
                     (if (= 1 (count unknowns))
                       (assoc res (first unknowns) allergen)
                       res)))
                 resolved
                 candidates))))))


(defn part-2
  [input]
  (->> (parse-input input)
       resolve-allergens
       (sort-by (comp name val))
       (map first)
       (string/join \,)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 5
  (part-1 task-input)                                       ; => 1945

  ;; Part 2
  (part-2 test-input)                                       ; => mxmxvkd,sqjhc,fvjkl
  (part-2 task-input)                                       ; => pgnpx,srmsh,ksdgk,dskjpq,nvbrx,khqsk,zbkbgp,xzb

  )
