(ns aoc-2019.intcode-interpreter
  (:require [aoc-utils :as u]
            [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn create-state
  [memory]
  {:ip     0
   :mem    memory
   :halt?  false
   :input  PersistentQueue/EMPTY
   :output []
   :steps  0})


(defn ^:private opcode
  [{:keys [mem ip]}]
  (-> ^long (nth mem ip)
      (rem 100)
      (abs)))


(defn ^:private instruction
  "Returns a 2 element vector:
  [vector-of-all-integers-in-the-instruction
   vector-of-parameter-modes]"
  [{:keys [mem ^long ip]} ^long instruction-length]
  (let [[^long code :as all] (subvec mem ip (+ ip instruction-length))
        modes (abs (quot code 100))]
    [all
     (loop [acc       []
            modes     modes
            remaining (dec instruction-length)]
       (if (zero? remaining)
         acc
         (recur
           (conj acc (rem modes 10))
           (quot modes 10)
           (dec remaining))))]))


(defn ^:private parameter
  ^long
  [{:keys [mem]} ^long p ^long mode]
  (case mode
    0 (nth mem p)
    1 p))


(defmulti ^:private execute-op
  "Execute the intcode operation specified by the value in memory that the instruction pointer
  is currently at."
  opcode)


(defn ^:private param-binding
  [state-sym param-sym mode-sym]
  (let [asterisked? (str/starts-with? (name param-sym) "*")]
    [param-sym
     (if asterisked?
       param-sym
       `(parameter ~state-sym ~param-sym ~mode-sym))]))

; Convenience macro to shorten the op implementation process.
; It takes the opcode, a vector of the state and the ops parameters, and a function body.
; Parameter names that start with `*` will always be used as is.
(defmacro defop
  [op [state-sym & param-syms] & body]
  (let [op (parse-long (re-find #"\d+" (name op)))
        num-params (count param-syms)
        instruction-size (inc num-params)
        mode-syms (mapv #(gensym (str (name %) "_mode_")) param-syms)]
    `(defmethod execute-op ~op
       [~state-sym]
       (-> (let [[[_# ~@param-syms] [~@mode-syms]] (instruction ~state-sym ~instruction-size)
                 ~@(mapcat #(param-binding state-sym %1 %2) param-syms mode-syms)]
             ~@body)
           (update :ip + ~instruction-size)))))


;; ADD
(defop CODE_1 [state a b *r]
  (update state :mem assoc *r (+ a b)))

; extends to something like:
#_(defmethod execute-op 1
  [state]
  (let [[[_ a b r] [a-mode b-mode]] (instruction state 4)
        a (parameter state a a-mode)
        b (parameter state b b-mode)]
    (-> (update state :mem assoc r (+ a b))
        (update :ip + 4))))


;; MUL
(defop CODE_2 [state a b *r]
  (update state :mem assoc *r (* a b)))


;; INPUT : Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
(defop CODE_3 [state *r]
  (let [value (peek (:input state))]
    (assert (some? value))
    (-> (update state :mem assoc *r value)
        (update :input pop))))


;; OUTPUT : Opcode 4 outputs the value of its only parameter.
(defop CODE_4 [state x]
  (update state :output conj x))


;; HALT
(defmethod execute-op 99
  [state]
  (assoc state :halt? true))


(defn step-program
  [state]
  (-> (execute-op state)
      (update :steps inc)))

(defn run-program
  [state]
  (loop [state state]
    (if (:halt? state)
      (do #_(u/debug "Steps executed: {state :steps}")
        state)
      (recur (step-program state)))))
