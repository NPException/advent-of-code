(ns aoc-2016.day-8-anim2
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
  [screen width height]
  (->> (for [x (range width)
             y (range height)]
         [y x])
       (reduce #(assoc-in %1 %2 \#) screen)))


(defn rotate-row
  "rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels.
  Pixels that would fall off the right end appear at the left end of the row."
  [screen y n]
  (let [src-row (nth screen y)]
    (loop [row src-row
           x   0]
      (if (= x 50)
        (assoc screen y row)
        (recur
          (assoc row (mod (+ x n) 50) (nth src-row x))
          (inc x))))))


(defn rotate-column
  "rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels.
  Pixels that would fall off the bottom appear at the top of the column."
  [screen x n]
  (loop [new-screen screen
         y          0]
    (if (= y 6)
      new-screen
      (recur
        (assoc-in new-screen
          [(mod (+ y n) 6) x]
          (get-in screen [y x]))
        (inc y)))))

; rect AxB
; rotate row y=A by B
; rotate column x=A by B

(defn parse-instructions
  [input]
  (->> (str/split-lines input)
       (mapv (fn [^String line]
               (let [[a b] (mapv parse-long (re-seq #"\d+" line))]
                 (cond
                   (str/starts-with? line "rect") #(rect % a b)
                   (str/starts-with? line "rotate row") #(rotate-row % a b)
                   (str/starts-with? line "rotate column") #(rotate-column % a b)
                   :else (throw (IllegalStateException. line))))))))



(defn part-1
  [input]
  (->> (parse-instructions input)
       (reductions #(%2 %1) blank-screen)
       (img/record-as-gif
         (doto (io/file "visualizations/aoc_2016/day_8_2.gif")
           (-> .getParentFile .mkdirs))
         20
         (img/on-off-image-fn 16 #(= % \#) [255, 116, 0] [128, 0, 72]))
       (last)
       (apply concat)
       (u/count-matching #(= % \#))))


(comment
  ;; Part 1
  (part-1 task-input)                                       ; => 106
  (time (part-1 task-input))

  )
