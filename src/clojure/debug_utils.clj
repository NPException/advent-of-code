(ns debug-utils
  (:require [clojure.main :as main])
  (:import (java.awt BorderLayout Dimension)
           (javax.swing JFrame JPanel JProgressBar UIManager)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


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


;; set system look and feel when the namespace is loaded
(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))


(defn spawn-progress-bar
  "Spawns a progress bar with the given name as the window title.
  `min` and `max` are the 0% and 100% integer values.
  Returns a function with the a current value to update the progress. That function will return the value."
  ([^String name min max]
   (spawn-progress-bar name min max 1))
  ([^String name ^long min ^long max ^long update-interval]
   (assert (pos? update-interval) "update-interval must be larger than 0")
   (let [bar (doto (JProgressBar. (int min) (int max))
               (.setStringPainted true))]
     ; create and show frame
     (doto (JFrame. name)
       (.setDefaultCloseOperation JFrame/HIDE_ON_CLOSE)
       (.add (doto (JPanel. (BorderLayout.))
               (.add bar BorderLayout/CENTER)
               (.setPreferredSize (Dimension. 800 50))))
       (.pack)
       (.setLocationRelativeTo nil)
       (.setVisible true))
     ; return function to update the progress-bar
     (if (= update-interval 1)
       ; update function without interval checks
       (fn [^long x]
         (.setValue bar (int x))
         x)
       ; update function with interval checks
       (fn [^long x]
         (when (or (>= x max) (zero? (rem (- x min) update-interval)))
           (.setValue bar (int x)))
         x)))))