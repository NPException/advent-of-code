(ns aoc-2016.day-5
  (:require [aoc-utils :as u]
            [clojure.string :as str]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 5: How About a Nice Game of Chess? ---

(def task-input "ojvtpuvg")

(def test-input "abc")


;; this is the `mine-advent-coin` portion copied and modified from 2015 Day 4

(defn hash-fn
  [door-id]
  (fn [n]
    (let [hash (u/md5 (str door-id n))]
      (when (str/starts-with? hash "00000")
        hash))))

(defn generate-hashes
  [door-id]
  (->> (iterate inc 0)
       (u/cpmap 1000 (hash-fn door-id))
       #_(map (hash-fn door-id hash-target))                ;; takes ~6x as long compared to u/cpmap
       (filter some?)))

(defn generate-password
  [door-id]
  (->> (generate-hashes door-id)
       (take 8)
       (map #(nth % 5))
       (apply str)))


(defn part-1
  [input]
  (generate-password input))


(defn part-2
  [input]
  (loop [[hash & more-hashes] (generate-hashes input)
         found    0
         password (vec (repeat 8 nil))]
    (if (= found 8)
      (apply str password)
      (let [pos  (parse-long (str (nth hash 5)))
            char (nth hash 6)]
        (if (and pos (< pos 8) (nil? (nth password pos)))
          (recur more-hashes (inc found) (assoc password pos char))
          (recur more-hashes found password))))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 18f47a30
  (part-1 task-input)                                       ; => 4543c154

  ;; Part 2
  (part-2 test-input)                                       ; => 05ace8e3
  (part-2 task-input)                                       ; => 1050cbbd

  )
