(ns debug_utils
  (:require [clojure.main :as main]))

;; a bit of repl magic from "The Joy of Clojure"
(defn contextual-eval [ctx expr]
  (eval
    `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
       ~expr)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap
      (map (fn [sym] `(quote ~sym)) symbols)
      symbols)))

(defn readr [prompt exit-code]
  (let [input (main/repl-read prompt exit-code)]
    (if (= input :exit)
      exit-code
      input)))

(defmacro break
  "Create a breakpoint that opens a repl with the local context.
  Entering `:exit` will continue the program."
  []
  `(main/repl
     :prompt #(print "debug=> ")
     :read readr
     :eval (partial contextual-eval (local-context))))



(defn inspect
  [x]
  (println x)
  x)
