(ns aoc-2021.day-18
  (:use [criterium.core])
  (:require [aoc-utils :as u]))

;; --- Day 18: Snailfish --- https://adventofcode.com/2021/day/18

(def task-input (u/read-as-vector (u/slurp-resource "inputs/aoc_2021/day-18.txt")))

(def test-input (u/read-as-vector "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))

#_(defn print-sn
  ([sn]
   (println (print-sn sn 0)))
  ([sn level]
   (let [level+ (inc level)]
       (cond
         (and (= level 4) (vector? sn))
         (str "<" (first sn) "," (second sn) ">")
         (vector? sn)
         (str "[" (print-sn (first sn) level+) "," (print-sn (second sn) level+) "]")
         :else
         (str (when (>= sn 10) "*") sn)))))


(defn find-path
  [node level path finalize-path]
  (or (finalize-path node level path)
      (when-let [[left right] (and (vector? node) node)]
        (or (find-path left (inc level) (conj path 0) finalize-path)
            (find-path right (inc level) (conj path 1) finalize-path)))))


(defn pop-while
  [path i]
  (loop [path path]
    (if (= i (peek path))
      (recur (pop path))
      path)))

(defn branch-to-number
  [sn path i]
  (loop [path path]
    (when-let [node (get-in sn path)]
      (if (number? node)
        path
        (recur (conj path i))))))

(defn find-number
  [sn path side]
  (let [path (pop-while path side)]
    (when (seq path)
      (branch-to-number
        sn
        (-> (pop path) (conj side))
        (bit-xor side 1)))))

(defn explode-sn
  "Returns the exploded snail number, if there was something to explode."
  ([sn]
   (when-let [path (find-path
                     sn 0 []
                     (fn [node level path]
                       (and (= level 4) (vector? node) path)))]
     (let [[a b] (get-in sn path)
           left-path (find-number sn path 0)
           right-path (find-number sn path 1)]
       (cond-> (assoc-in sn path 0)
         left-path (update-in left-path + a)
         right-path (update-in right-path + b))))))


(defn split-sn
  [sn]
  (when-let [path (find-path
                    sn 0 []
                    (fn [node _level path]
                      (and (number? node) (>= node 10) path)))]
    (let [n (get-in sn path)
          low (quot n 2)]
      (assoc-in sn path [low (- n low)]))))


(defn reduce-sn
  [sn]
  (loop [sn sn]
    (if-let [exploded (explode-sn sn)]
      (recur exploded)
      (if-let [split (split-sn sn)]
        (recur split)
        sn))))


(defn magnitude
  [sn]
  (if-let [[a b] (and (vector? sn) sn)]
    (+ (* 3 (magnitude a))
       (* 2 (magnitude b)))
    sn))


(defn part-1
  [input]
  (magnitude (reduce #(reduce-sn [%1 %2]) input)))


(defn part-2
  [input]
  (let [size (count input)]
    (->> (for [a (range size)
               :let [sn-a (nth input a)]]
           (for [b (range (inc a) size)
                 :let [sn-b (nth input b)]]
             [(magnitude (reduce-sn [sn-a sn-b]))
              (magnitude (reduce-sn [sn-b sn-a]))]))
         flatten
         (apply max))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 4140
  (part-1 task-input)                                       ; => 3051
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 3993
  (part-2 task-input)                                       ; => 4812
  (quick-bench (part-2 task-input))

  )
