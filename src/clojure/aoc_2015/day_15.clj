(ns aoc-2015.day-15
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 15: Science for Hungry People --- https://adventofcode.com/2015/day/15

(def task-input (u/slurp-resource "inputs/aoc_2015/day-15.txt"))

(def test-input "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")


(defn parse-line
  [line]
  (let [[name attribs] (string/split line #": ")]
    [name (->> (read-string (str "[" attribs "]"))
               (map #(if (symbol? %) (keyword %) %))
               (apply hash-map))]))

(defn parse-input
  [input]
  (into {} (map parse-line) (string/split-lines input)))


;; part 1 functions

(defn score
  [{:keys [flavor capacity durability texture] :as _recipe}]
  (if (or (<= flavor 0) (<= capacity 0) (<= durability 0) (<= texture 0))
    0
    (* flavor capacity durability texture)))


(defn build-recipe-starter
  [ingredients]
  (reduce-kv
    (fn [acc _k v]
      (merge-with + acc v))
    {}
    ingredients))


(defn add-optimal-ingredient
  [recipe ingredients]
  (->> ingredients
       (eduction (comp (map val)
                       (map #(merge-with + recipe %))))
       (apply max-key score)))


(defn part-1
  [input]
  (let [ingredients (parse-input input)]
    (loop [n (- 100 (count ingredients))
           recipe (build-recipe-starter ingredients)]
      (if (zero? n)
        (score recipe)
        (recur (dec n) (add-optimal-ingredient recipe ingredients))))))


(defn multiply-ingredient
  [ingredient amount]
  (into {} (mapv #(vector (key %) (* (val %) amount)) ingredient)))

(defn build-recipe
  [ingredients amounts]
  (->> (mapv multiply-ingredient ingredients amounts)
       (apply merge-with +)))

(defn part-2
  [input]
  (let [ingredients (vec (vals (parse-input input)))]
    (->> (u/partitions 100 (count ingredients))
         (map #(build-recipe ingredients %))
         (filter #(= (:calories %) 500))
         (map score)
         (apply max))))


(comment
  {"Frosting"     {:calories 5, :flavor  0, :capacity  4, :durability -2, :texture 0},
   "Candy"        {:calories 8, :flavor -1, :capacity  0, :durability  5, :texture 0},
   "Butterscotch" {:calories 6, :flavor  5, :capacity -1, :durability  0, :texture 0},
   "Sugar"        {:calories 1, :flavor -2, :capacity  0, :durability  0, :texture 2}}


  ;; Part 1
  (part-1 test-input)                                       ; => 62842880
  (part-1 task-input)                                       ; => 18965440

  ;; Part 2
  (part-2 test-input)                                       ; => 57600000
  (part-2 task-input)                                       ; => 15862900

  )
