(ns aoc-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [org.httpkit.client :as http])
  (:import [java.time LocalDateTime]))


(defn slurp-resource
  [path]
  (slurp (io/resource path)))


(defn inspect
  [x]
  (println x)
  x)


(defn parse-int
  "Pares the given String to an integer. If it cannot be parsed, returns nil."
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _)))


(defn cpmap
  "A chunked variant of pmap. Instead of using one thread for application of f,
  uses one thread for n applications of f."
  [n f col]
  (->> (partition-all n col)
       (pmap #(mapv f %))
       (apply concat)))



(defn start-day
  "Initializes a new namespace for the given day and downloads the input for the day.
  This requires a valid session id in the environment variable 'AOC_SESSION'."
  ([]
   (let [now (LocalDateTime/now)]
     (start-day (-> now .getYear) (-> now .getDayOfMonth))))
  ([year day]
   ;; create input text file
   (let [inputs-file (io/file (str "./resources/inputs/aoc_" year "/day-" day ".txt"))
         input (:body @(http/get (str "https://adventofcode.com/" year "/day/" day "/input")
                                 {:headers {"cookie" (str "session=" (System/getenv "AOC_SESSION"))}}))]
     (-> inputs-file .getParentFile .mkdirs)
     (spit inputs-file (string/trim-newline input)))
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