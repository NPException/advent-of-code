(ns aoc-utils)

(defn parse-int
  "Pares the given String to an integer. If it cannot be parsed, returns nil."
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _)))
