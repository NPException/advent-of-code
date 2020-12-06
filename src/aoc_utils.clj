(ns aoc-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.time LocalDateTime]))


(defn slurp-resource
  [path]
  (slurp (io/resource path)))


(defn parse-int
  "Pares the given String to an integer. If it cannot be parsed, returns nil."
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _)))




(defn start-day
  ([]
   (let [now (LocalDateTime/now)]
     (start-day (-> now .getYear) (-> now .getDayOfMonth))))
  ([year day]
   ;; create input text file
   (let [inputs-file (io/file (str "./resources/inputs/aoc_" year "/day-" day ".txt"))]
     (-> inputs-file .getParentFile .mkdirs)
     (.createNewFile inputs-file))
   ;; create clojure namespace file
   (let [ns-file (io/file (str "./src/aoc_" year "/day_" day ".clj"))]
     (-> ns-file .getParentFile .mkdirs)
     (when (.createNewFile ns-file)
       (-> (slurp-resource "dummy_ns.edn")
           (string/replace #"%>.+?<%" {"%>year<%" (str year)
                                       "%>day<%"  (str day)})
           (#(spit ns-file %)))))))

(comment
  (start-day)
  )