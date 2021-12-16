(ns aoc-2021.day-16
  (:use [criterium.core])
  (:require [aoc-utils :as u]))

;; --- Day 16: Packet Decoder --- https://adventofcode.com/2021/day/16

(def task-input (u/slurp-resource "inputs/aoc_2021/day-16.txt"))

(def hex->binary
  (->> [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A \B \C \D \E \F]
       (map-indexed (fn [i c]
                      (let [binary (Long/toBinaryString i)]
                        [c (->> (concat (repeat (- 4 (count binary)) \0) binary)
                                (mapv #(- (int %) 48)))])))
       (into {})))

(defn bits->num
  [bits]
  (Long/parseLong (apply str bits) 2))


(defn read-literal
  [bits]
  (loop [[^int a b c d e & bits] bits
         num-bits []]
    (if (zero? a)
      [(bits->num (conj num-bits b c d e))
       bits]
      (recur bits (conj num-bits b c d e)))))


(declare read-packet)

(defn read-all-packets
  [bits]
  (loop [bits bits
         sub-packets []]
    (if (empty? bits)
      sub-packets
      (let [[packet sub-bits] (read-packet bits)]
        (recur sub-bits (conj sub-packets packet))))))

(defn read-n-packets
  [bits n]
  (loop [i 0
         bits bits
         sub-packets []]
    (if (= i n)
      [sub-packets bits]
      (let [[packet bits] (read-packet bits)]
        (recur (inc i) bits (conj sub-packets packet))))))

(defn read-operator
  [[^int length-type & bits]]
  (if (zero? length-type)
    (let [[num-sub-bits bits] (split-at 15 bits)
          [sub-bits non-op-bits] (split-at (bits->num num-sub-bits) bits)]
      [(read-all-packets sub-bits)
       non-op-bits])
    (let [[num-binary bits] (split-at 11 bits)]
      (read-n-packets bits (bits->num num-binary)))))


(defn read-packet
  [bits]
  (let [[v1 v2 v3 t1 t2 t3 & bits] bits
        version (bits->num [v1 v2 v3])
        type-id (bits->num [t1 t2 t3])
        [content bits] (if (= type-id 4)
                         (read-literal bits)
                         (read-operator bits))]
    [{:version version
      :type-id type-id
      :content content}
     bits]))


(defn solve-packet
  [input analyze-fn]
  (->> (mapcat hex->binary input)
       read-packet
       first
       analyze-fn))

(defn part-1
  [input]
  (solve-packet input
                (fn version-sum [{:keys [version content]}]
                  (if (number? content)
                    version
                    (apply + version (map version-sum content))))))

(defn part-2
  [input]
  (solve-packet input
                (fn eval-packet
                  [{:keys [type-id content]}]
                  (if (= type-id 4)
                    content
                    (let [[^long a ^long b :as values] (map eval-packet content)]
                      (case (int type-id)
                        0 (apply + values)
                        1 (apply * values)
                        2 (apply min values)
                        3 (apply max values)
                        5 (if (> a b) 1 0)
                        6 (if (< a b) 1 0)
                        7 (if (= a b) 1 0)))))))


(comment
  ;; Part 1
  (part-1 "A0016C880162017C3686B18A3D4780")                 ; => 31
  (part-1 task-input)                                       ; => 906
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 "9C0141080250320F1802104A08")                     ; => 1
  (part-2 task-input)                                       ; => 819324480368
  (quick-bench (part-2 task-input))

  )
