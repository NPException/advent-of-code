(ns aoc-2020.day-14
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 14:  --- https://adventofcode.com/2020/day/14

(def task-input (u/slurp-resource "inputs/aoc_2020/day-14.txt"))

(def test-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0")
(def test-input-2 "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1")


(defn find-floaty-bits
  [mask-string]
  (->> (map-indexed vector mask-string)
       (filter #(= \X (second %)))
       (mapv first)))

(defn parse-command
  [[op arg-1 arg-2]]
  (case op
    "mask" [:mask
            (-> arg-2 (string/replace \X \0) u/parse-binary)
            (-> arg-2 (string/replace \X \1) u/parse-binary)
            (find-floaty-bits arg-2)]
    "mem" [:mem
           (u/parse-long arg-1)
           (u/parse-long arg-2)]))


(defn parse-program
  [input]
  (->> (string/split-lines input)
       (map (comp rest #(re-matches #"(mask|mem)\[?(\d*?)\]? = (\w+)" %)))
       (mapv parse-command)))


(defn do-task
  [input step-fn]
  (let [program (parse-program input)
        vm {:memory {}}]
    (->> program
         (reduce step-fn vm)
         :memory
         vals
         (apply +))))


;; part 1

(defmulti step-program-1 (fn [_ [op _ _]] op))

(defmethod step-program-1 :mask
  [vm [_ mask-1 mask-0]]
  (-> vm
      (assoc :mask-1 mask-1)
      (assoc :mask-0 mask-0)))

(defmethod step-program-1 :mem
  [{:keys [mask-1 mask-0] :as vm} [_ address value]]
  (update vm :memory
          assoc
          address
          (-> value
              (bit-or mask-1)
              (bit-and mask-0))))


(defn part-1
  [input]
  (do-task input step-program-1))


;; part-2

(defn decode-addresses
  [{:keys [mask-1 floaty-bits floaty-permutations]} address]
  (let [binary-address (Long/toBinaryString (bit-or mask-1 address))
        address-bits (-> (repeat (- 36 (count binary-address)) \0)
                         (concat binary-address)
                         vec)]
    (for [n (range floaty-permutations)]
      (->> floaty-bits
           (reduce-kv
             (fn [bin-vec float-index bit-index]
               (if (-> (bit-shift-left 1 float-index)
                       (bit-and n)
                       (> 0))
                 (assoc! bin-vec bit-index \1)
                 (assoc! bin-vec bit-index \0)))
             (transient address-bits))
           persistent!
           string/join
           u/parse-binary))))


(defmulti step-program-2 (fn [_ [op _ _]] op))

(defmethod step-program-2 :mask
  [vm [_ mask-1 mask-0 floaty-bits]]
  (-> vm
      (assoc :mask-1 mask-1)
      (assoc :mask-0 mask-0)
      (assoc :floaty-bits floaty-bits)
      (assoc :floaty-permutations (apply * (repeat (count floaty-bits) 2)))))

(defmethod step-program-2 :mem
  [vm [_ address value]]
  (reduce
    #(update %1 :memory assoc %2 value)
    vm
    (decode-addresses vm address)))


(defn part-2
  [input]
  (do-task input step-program-2))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 165
  (part-1 task-input)                                       ; => 15172047086292

  ;; Part 2
  (part-2 test-input-2)                                     ; => 208
  (part-2 task-input)                                       ; => 4197941339968

  )
