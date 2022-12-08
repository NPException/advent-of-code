(ns aoc-2022.day-7
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

;; --- Day 7: No Space Left On Device ---

(def task-input (u/slurp-resource "inputs/aoc_2022/day-7.txt"))

(def test-input "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k")


(defn cd
  [path arg]
  (case arg
    "/" ["/"]
    ".." (pop path)
    (conj path arg)))

;ls means list. It prints out all of the files and directories immediately contained by the current directory:
;  123 abc means that the current directory contains a file named abc with size 123.
;  dir xyz means that the current directory contains a directory named xyz.
(defn ls
  [fs path lines]
  (let [[listings more-lines] (split-with #(not= (first %) "$") lines)]
    [(reduce
       (fn [fs [size name]]
         (update-in fs path
           assoc name (if (= size "dir")
                        {}
                        (parse-long size))))
       fs
       listings)
     path
     more-lines]))


(defn parse-filesystem
  ([lines]
   (parse-filesystem {} ["/"] lines))
  ([fs path [[_ cmd arg :as line] & more-lines]]
   (if-not line
     fs
     (let [[fs' path' lines'] (case cmd
                                "cd" [fs (cd path arg) more-lines]
                                "ls" (ls fs path more-lines))]
       (recur fs' path' lines')))))


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv #(str/split % #" "))
       (parse-filesystem)))


(defn directory-sizes
  "Returns a vector of [size-of-dir, map-of-sub-directory-sizes]"
  ([fs] (directory-sizes fs {} ["/"]))
  ([fs sizes path]
   (let [dir (get-in fs path)]
     (if (every? int? (vals dir))
       ;; leaf directory. every child is a file
       (let [size (apply + (vals dir))]
         [size (assoc sizes path size)])
       ;; has further subdirectories
       (let [[size new-sizes] (reduce
                                (fn [[acc-size sizes] [name val]]
                                  (let [[size-to-add new-sizes] (if (int? val)
                                                                  [val sizes]
                                                                  (directory-sizes fs sizes (conj path name)))]
                                    [(+ acc-size size-to-add) new-sizes]))
                                [0 sizes]
                                dir)]
         [size (assoc new-sizes path size)])))))


(defn part-1
  [input]
  (->> (parse-input input)
       (directory-sizes)
       (second)
       (map val)
       (filter #(<= % 100000))
       (apply +)))


(defn part-2
  [input]
  (let [total-mem 70000000
        [used sizes] (directory-sizes (parse-input input))
        unused (- total-mem used)
        needed (- 30000000 unused)]
    (->> (vals sizes)
         (filter #(>= % needed))
         (apply min))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 95437
  (part-1 task-input)                                       ; => 1517599
  (crit/quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 24933642
  (part-2 task-input)                                       ; => 2481982
  (crit/quick-bench (part-2 task-input))

  )
