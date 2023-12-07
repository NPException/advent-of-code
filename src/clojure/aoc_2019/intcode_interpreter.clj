(ns aoc-2019.intcode-interpreter
  (:require [aoc-utils :as u]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn create-state
  [memory]
  {:ip           0
   :mem          (apply vector-of :long memory)
   :rel-base     0
   :halt?        false
   :input        (vector-of :long)
   :needs-input? false
   :output       (vector-of :long)
   :steps        0})


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
  "Reads the given parameter from the memory of `state` based on the given parameter mode."
  ^long
  [{:keys [mem ^long rel-base]} ^long p ^long mode]
  (case mode
    0 (nth mem p 0)
    1 p
    2 (nth mem (+ rel-base p) 0)))


(defn ^:private write-address
  "Transforms the given write address parameter according to the given parameter mode."
  ^long
  [{:keys [mem ip ^long rel-base]} ^long p ^long mode]
  (case mode
    0 p
    1 (throw (ex-info "parameter mode 1 not supported for write addresses" {:opcode (nth mem ip), :param p}))
    2 (+ rel-base p)))


(defmulti ^:private execute-op
  "Execute the intcode operation specified by the value in memory that the instruction pointer
  is currently at."
  opcode)


(defn ^:private param-binding
  [state-sym param-sym mode-sym]
  (let [asterisked? (str/starts-with? (name param-sym) "*")]
    [param-sym
     (if asterisked?
       `(write-address ~state-sym ~param-sym ~mode-sym)
       `(parameter ~state-sym ~param-sym ~mode-sym))]))


(defn assoc-expanding
  [mem ^long index value]
  (let [size (count mem)]
    (when (> index 10000)
      (println "We're getting big: " index))
    (assoc (cond-> mem
             (> index size)
             (into (long-array (- index size))))
      index value)))


; Convenience macro to shorten the op implementation process.
; It takes the opcode, a vector of the state and the ops parameters, and a function body.
; Parameter names that start with `*` indicate write addresses.
(defmacro defop
  [op [state-sym & param-syms] & body]
  (let [op               (parse-long (re-find #"\d+" (name op)))
        num-params       (count param-syms)
        instruction-size (inc num-params)
        mode-syms        (mapv #(gensym (str (name %) "_mode_")) param-syms)]
    `(defmethod execute-op ~op
       [~state-sym]
       (-> (let [[[_# ~@param-syms] [~@mode-syms]] (instruction ~state-sym ~instruction-size)
                 ~@(mapcat #(param-binding state-sym %1 %2) param-syms mode-syms)]
             ~@body)
           (update :ip + ~instruction-size)))))


;; ADD
(defop CODE_1 [state a b *r]
  (update state :mem assoc-expanding *r (+ a b)))

; extends to something like:
#_(defmethod execute-op 1
    [state]
    (let [[[_ a b r] [a-mode b-mode]] (instruction state 4)
          a (parameter state a a-mode)
          b (parameter state b b-mode)]
      (-> (update state :mem assoc-expanding r (+ a b))
          (update :ip + 4))))


;; MUL
(defop CODE_2 [state a b *r]
  (update state :mem assoc-expanding *r (* a b)))


;; INPUT : Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
(defop CODE_3 [state *r]
  (let [[value] (:input state)]
    (if value
      (-> (update state :mem assoc-expanding *r value)
          (assoc :needs-input? false)
          (update :input subvec 1))
      ; pause execution to wait for input and compensate for automatic IP increase by the `defop` macro
      (-> (assoc state :needs-input? true)
          (update :ip - 2)))))


;; OUTPUT : Opcode 4 outputs the value of its only parameter.
(defop CODE_4 [state x]
  (update state :output conj x))


;; JUMP_IF_TRUE
(defop CODE_5 [state x jmp-target]
  (cond-> state
    ;; compensate jump for automatic IP increase by the `defop` macro
    (not (zero? x)) (assoc :ip (- jmp-target 3))))


;; JUMP_IF_FALSE
(defop CODE_6 [state x jmp-target]
  (cond-> state
    ;; compensate jump for automatic IP increase by the `defop` macro
    (zero? x) (assoc :ip (- jmp-target 3))))


;; LESS_THAN
(defop CODE_7 [state a b *r]
  (update state :mem assoc-expanding *r (if (< a b) 1 0)))


;; EQUALS
(defop CODE_8 [state a b *r]
  (update state :mem assoc-expanding *r (if (== a b) 1 0)))


;; ADJUST_RELATIVE_BASE
(defop CODE_9 [state x]
  (update state :rel-base + x))


;; HALT
(defop CODE_99 [state]
  (-> (assoc state :halt? true)
      (update :ip dec)))                                    ; compensate for automatic IP increase by `defop` macro


(defn step-program
  [state]
  (-> (execute-op state)
      (update :steps inc)))

(defn run-program
  [state]
  (loop [state (step-program state)]
    (if (or (:halt? state)
            (:needs-input? state))
      state
      (recur (step-program state)))))


(defn push-input
  [state input-value]
  (update state :input conj input-value))

(defn push-output
  [state output-value]
  (update state :output conj output-value))

(defn last-output
  [state]
  (peek (:output state)))



(comment

  (defn test-program
    [mem]
    (-> (create-state mem)
        (run-program)
        :output))

  ; [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] takes no input and produces a copy of itself as output.
  (= (test-program [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99])
     [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99])

  ; [1102,34915192,34915192,7,4,7,99,0] should output a 16-digit number.
  (= (test-program [1102, 34915192, 34915192, 7, 4, 7, 99, 0])
     [1219070632396864])

  ; [104,1125899906842624,99] should output the large number in the middle.
  (= (test-program [104,1125899906842624,99])
     [1125899906842624])


  ; Run the BOOST program from Day 9 in test mode (1) to check validity of opcodes.
  ; It will perform a series of checks on each opcode, output any opcodes (and the associated parameter modes)
  ; that seem to be functioning incorrectly, and finally output a BOOST keycode.
  ; Once your Intcode computer is fully functional, the BOOST program should report
  ; no malfunctioning opcodes when run in test mode; it should only output a single value, the BOOST keycode.
  (-> (u/read-as-vector (u/slurp-resource "inputs/aoc_2019/day-9.txt"))
      (create-state)
      (push-input 1)
      (run-program)
      :output)
  ; => [2427443564]

  )