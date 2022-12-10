(ns aoc-2022.day-10
  (:require [aoc-utils :as u]
            [clojure.string :as str]
            [criterium.core :as crit]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; --- Day 10: Cathode-Ray Tube ---


(defmacro noop [x] x)


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map u/read-as-vector)
       (mapcat (fn [[opcode arg]]
                 (if (= opcode 'addx)
                   [`(noop) `(+ ~arg)]
                   [`(noop)])))))


(def task-program (parse-input (u/slurp-resource "inputs/aoc_2022/day-10.txt")))

(def test-program (parse-input "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"))


(defn update-signal-strength
  ^long [strength-sum ^long cycle ^long x]
  (vswap! strength-sum #(+ ^long % (* cycle x)))
  x)

(defn compile-part-1
  [program]
  (let [signal-strength (gensym "strength-")]
    `(let [~signal-strength (volatile! 0)]
       (->> 1
            ~@(->> program
                   (map-indexed (fn [^long i op] [(inc i) op]))
                   (mapcat
                     (fn [[^long cycle op]]
                       (if (or (= cycle 20)
                               (and (> cycle 20)
                                    (= 0 (mod (- cycle 20) 40))))
                         [`(update-signal-strength ~signal-strength ~cycle) op]
                         [op])))))
       (deref ~signal-strength))))



(defmacro part-1-test []
  (compile-part-1 test-program))

(defmacro part-1 []
  (compile-part-1 task-program))


(defn new-line
  ^long [^long x]
  (println)
  x)

(defn draw
  ^long [^long cycle ^long x]
  (let [pixel (mod (dec cycle) 40)]
    (print (if (<= (dec x) pixel (inc x))
             \# \space)))
  x)

(defn compile-part-2
  [program]
  `(->> 1
        ~@(->> program
               (map-indexed (fn [^long i op] [(inc i) op]))
               (mapcat
                 (fn [[cycle op]]
                   (if (= 0 (mod cycle 40))
                     [`(draw ~cycle) `(new-line) op]
                     [`(draw ~cycle) op]))))))

(defmacro part-2-test []
  (compile-part-2 test-program))

(defmacro part-2 []
  (compile-part-2 task-program))


(comment
  ;; Part 1
  (part-1-test)                                             ; => 13140
  (part-1)                                                  ; => 14320
  (crit/quick-bench (part-1))

  ;; Part 2
  (part-2-test)
  ; => ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  
  ;    ###   ###   ###   ###   ###   ###   ### 
  ;    ####    ####    ####    ####    ####    
  ;    #####     #####     #####     #####     
  ;    ######      ######      ######      ####
  ;    #######       #######       #######     
  (part-2)                                                  ; => PCPBKAPJ
  (crit/quick-bench (with-out-str (part-2)))

  )
