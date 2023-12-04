(ns aoc-2019.intcode-interpreter
  (:require [aoc-utils :as u]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn create-state
  [input-string]
  {:ip    0
   :mem   (u/read-as-vector input-string)
   :steps 0
   :halt? false})


(defn ^:private op-code
  [{:keys [mem ip]}]
  (nth mem ip))


(defn ^:private fetch
  ^long
  [mem ^long i]
  (nth mem i))


(defmulti ^:private execute-op
  "Execute the intcode operation specified by the value in memory that the instruction pointer
  is currently at."
  op-code)

;; ADD
(defmethod execute-op 1
  [{:keys [mem ^long ip] :as state}]
  (let [[_ *a *b *r] (subvec mem ip (+ ip 4))
        a (fetch mem *a)
        b (fetch mem *b)
        r (+ a b)]
    (-> (update state :mem assoc *r r)
        (update :ip + 4))))

;; MUL
(defmethod execute-op 2
  [{:keys [mem ^long ip] :as state}]
  (let [[_ *a *b *r] (subvec mem ip (+ ip 4))
        a (fetch mem *a)
        b (fetch mem *b)
        r (* a b)]
    (-> (update state :mem assoc *r r)
        (update :ip + 4))))

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
      (do (u/debug "Steps executed: {state :steps}")
          state)
      (recur (step-program state)))))
