(ns aoc-2020.day-13
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 13: Shuttle Search --- https://adventofcode.com/2020/day/13

(def task-input (u/slurp-resource "inputs/aoc_2020/day-13.txt"))

(def test-input "939\n7,13,x,x,59,x,31,19")


(defn parse-input
  [^String input]
  (let [[time & bus-ids] (string/split input #"[\n,]")]
    [(u/parse-long time)
     (mapv u/parse-long bus-ids)]))


;; part 1

(defn minutes-to-wait
  [time bus-id]
  (- bus-id (mod time bus-id)))


(defn part-1
  [input]
  (let [[time bus-ids] (parse-input input)]
    (->> (remove nil? bus-ids)
         (sort-by #(minutes-to-wait time %))
         first
         (#(* % (minutes-to-wait time %))))))


;; part 2

;; chinese remainder code nicked from rosetta code and adjusted
;; Jezza only found this through a Wikipedia rabbit hole.
;; How on earth are we supposed to know this stuff?

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(u/absl b) 0 1]
        (zero? b) [(u/absl a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (u/absl b)
                     r0 (u/absl a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese_remainder
  "Main routine to return the chinese remainder "
  [a n]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)
                        egcd (extended-gcd p n_i)
                        inv_p (second egcd)]
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]
    (mod sum-prod prod)))


(defn parse-buses
  [input]
  (->> (parse-input input)
       second
       (map-indexed vector)
       (remove (comp nil? second))))

;; probably relevant note on part 2: all bus ids are prime
(defn part-2
  [input]
  (->> (parse-buses input)
       (map (fn [[i id]]
              [(- id i) id]))
       (reduce
         (fn [[as ns] [a n]]
           [(conj as a) (conj ns n)])
         [[] []])
       (apply chinese_remainder)))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 295
  (part-1 task-input)                                       ; => 2092

  ;; Part 2
  (part-2 test-input)                                       ; => 1068781
  (part-2 task-input)                                       ; => 702970661767766

  (map
    #(part-2 (str "\n" %))
    ["17,x,13,19"                                           ; 3417
     "67,7,59,61"                                           ; 754018
     "67,x,7,59,61"                                         ; 779210
     "67,7,x,59,61"                                         ; 1261476
     "1789,37,47,1889"])                                    ; 1202161486
  )
