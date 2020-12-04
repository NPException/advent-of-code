(ns aoc-utils
  (:require [clojure.java.io :as io]))


(defn slurp-resource
  [path]
  (slurp (io/resource path)))


(defn parse-int
  "Pares the given String to an integer. If it cannot be parsed, returns nil."
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _)))
