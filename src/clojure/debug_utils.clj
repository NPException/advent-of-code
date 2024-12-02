(ns debug-utils
  (:require [clojure.edn :as edn]
            [clojure.main :as main]
            [clojure.string :as str])
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



(defn parse-debug-value
  [[_ sym-name entries]]
  (if (nil? entries)
    (symbol sym-name)
    (let [entries (mapv
                    #(list 'quote %)
                    (edn/read-string
                      (if (str/starts-with? entries "[")
                        entries
                        (str "[" entries "]"))))]
      `(get-in ~(symbol sym-name) ~entries))))

(defmacro debug
  "println with automatically resolving placeholders.
  {x} - Resolves to the current binding of x.
  {x k} - Resolves the key 'k' in the associative data structure x.
          The key is taken literally. To check for a key in form of
          a keyword, symbol, or string, use :k, k, or \"k\" respectively.
  {x [a b]} - Similar to {x k}, but resolves via 'get-in'."
  [^String s]
  (let [placeholders  (re-seq #"\{([^{} ,]+)(?: ([^{} ,]+|\[(?:[^{} ,]+[, ]*)+\]))?\}" s)
        format-string (reduce #(str/replace-first %1 (first %2) "%s") s placeholders)
        values        (->> placeholders
                           (map parse-debug-value)
                           (map #(list 'clojure.core/pr-str %)))]
    `(println (format ~format-string ~@values))))



;; set system look and feel when the namespace is loaded
(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

(definterface ProgressBar
  (^long set [^long x] "Sets the current progress with a value that lies between min and max. Returns x.")
  (hide [] "Closes the progress bar window.")
  (show [] "Shows the progress bar window."))

(defn spawn-progress-bar
  "Spawns a progress bar with the given name as the window title.
  `min` and `max` are the 0% and 100% integer values.
  Returns a function which takes the current value to update the progress.
  That function will return the value it was given."
  (^ProgressBar [^String name min max]
   (spawn-progress-bar name min max 1))
  (^ProgressBar [^String name ^long min ^long max ^long update-interval]
   (assert (pos? update-interval) "update-interval must be larger than 0")
   (let [bar   (doto (JProgressBar. (int min) (int max))
                 (.setStringPainted true))
         ; create and show frame
         frame (doto (JFrame. name)
                 (.setDefaultCloseOperation JFrame/HIDE_ON_CLOSE)
                 (.add (doto (JPanel. (BorderLayout.))
                         (.add bar BorderLayout/CENTER)
                         (.setPreferredSize (Dimension. 800 50))))
                 (.pack)
                 (.setLocationRelativeTo nil)
                 (.setVisible true))
         always-update? (= update-interval 1)]
     ; return ProgressBar
     (reify ProgressBar
       (^long set [_ ^long x]
         (when (or always-update?
                   (>= x max)
                   (zero? (rem (- x min) update-interval)))
           (.setValue bar (int x)))
         x)
       (hide [_]
         (.setVisible frame false)
         (.dispose frame))
       (show [_]
         (.pack frame)
         (.setVisible frame true))))))
