(ns aoc-2019.intcode-interpreter
  (:require [aoc-utils :as u]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn create-state
  [memory]
  {:ip    0
   :mem   memory
   :steps 0
   :halt? false})


(defn ^:private opcode
  [{:keys [mem ip]}]
  (nth mem ip))

(defn ^:private instruction
  [{:keys [mem ^long ip]} ^long instruction-length]
  (subvec mem ip (+ ip instruction-length)))


(defn ^:private memory-at
  ^long
  [{:keys [mem]} ^long i]
  (nth mem i))


(defmulti ^:private execute-op
  "Execute the intcode operation specified by the value in memory that the instruction pointer
  is currently at."
  opcode)

; TODO Write a convenience macro `defop` to shorten the op implementation process.
;      It that takes a vector of n+1 elements (the state, the op itself, and its parameters)
;      as well as a function body.
;      Parameter names that start with `*` will use the parameter as a pointer and bind
;      the value at that address to the name.
(comment
  ; example for ADD op
  (defop 1 [state _ *a *b r]
    (update state :mem assoc r (+ *a *b)))

  ; will expand to:
  (defmethod execute-op 1
    [state]
    (let [[_ *a6593 *b6598 r] (instruction state 4)
          *a (memory-at state *a6593)
          *b (memory-at state *b6598)]
      (-> (update state :mem assoc r (+ *a *b))
          (update :ip + 4))))
  ;
  )

;; ADD
(defmethod execute-op 1
  [state]
  (let [[_ *a *b r] (instruction state 4)
        a (memory-at state *a)
        b (memory-at state *b)]
    (-> (update state :mem assoc r (+ a b))
        (update :ip + 4))))

;; MUL
(defmethod execute-op 2
  [state]
  (let [[_ *a *b *r] (instruction state 4)
        a (memory-at state *a)
        b (memory-at state *b)
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
      (do #_(u/debug "Steps executed: {state :steps}")
          state)
      (recur (step-program state)))))
