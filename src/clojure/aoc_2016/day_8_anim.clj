(ns aoc-2016.day-8-anim
  (:require [aoc-utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [image-utils :as img]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 8: Two-Factor Authentication ---

(def task-input (u/slurp-resource "inputs/aoc_2016/day-8.txt"))


(def blank-screen
  (vec (repeat 6 (vec (repeat 50 \space)))))


(defn rect
  "rect AxB turns on all of the pixels in a rectangle
   at the top-left of the screen which is A wide and B tall."
  [record screen width height]
  (->> (for [x (range width)
             y (range height)]
         [y x])
       (reduce #(record (assoc-in %1 %2 \#)) screen)))


(defn rotate-row
  "rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels.
  Pixels that would fall off the right end appear at the left end of the row."
  [record screen y n]
  (if (> n 1)
    (nth (iterate
           #(rotate-row record % y 1)
           screen)
      n)
    (let [src-row (nth screen y)]
      (loop [row src-row
             x   0]
        (if (= x 50)
          (record (assoc screen y row))
          (recur
            (assoc row (mod (+ x n) 50) (nth src-row x))
            (inc x)))))))


(defn rotate-column
  "rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels.
  Pixels that would fall off the bottom appear at the top of the column."
  [record screen x n]
  (if (> n 1)
    (nth (iterate
           #(rotate-column record % x 1)
           screen)
      n)
    (loop [new-screen screen
           y          0]
      (if (= y 6)
        (record new-screen)
        (recur
          (assoc-in new-screen
            [(mod (+ y n) 6) x]
            (get-in screen [y x]))
          (inc y))))))

; rect AxB
; rotate row y=A by B
; rotate column x=A by B

(defn parse-instructions
  [input]
  (->> (str/split-lines input)
       (mapv (fn [^String line]
               (let [[a b] (mapv parse-long (re-seq #"\d+" line))]
                 (cond
                   (str/starts-with? line "rect") #(rect %1 %2 a b)
                   (str/starts-with? line "rotate row") #(rotate-row %1 %2 a b)
                   (str/starts-with? line "rotate column") #(rotate-column %1 %2 a b)
                   :else (throw (IllegalStateException. line))))))))


(defn create-recorder
  []
  (img/start-gif-recorder
    (let [f (io/file "visualizations/aoc_2016/day_8.gif")]
      (.mkdirs (.getParentFile f))
      f)
    (quot 1000 30)
    0
    (partial img/image-from-data
      #(if (= % \#)
         [255, 116, 0]
         [128, 0, 72])
      16)))


(defn record-times
  [record n data]
  (dotimes [_ n]
    (record data))
  data)


(defn part-1
  [input]
  (let [gif    (create-recorder)
        record #(img/record-frame! gif %)
        result (->> (parse-instructions input)
                    (reduce
                      (fn [screen instruction]
                        (instruction record screen))
                      (record blank-screen))
                    ;; few more frames so the result can be seen
                    (record-times record 60)
                    ;; continue with result
                    (apply concat)
                    (u/count-matching #(= % \#)))]
    (img/finish-recording! gif)
    result))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 106
  (time (part-1 task-input))

  )
