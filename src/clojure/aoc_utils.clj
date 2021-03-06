(ns aoc-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.main :as main]
            [org.httpkit.client :as http])
  (:import [java.time LocalDateTime]))


(defn slurp-resource
  [path]
  (slurp (io/resource path)))


(defn inspect
  [x]
  (println x)
  x)

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
    (if (= input ::exit)
      exit-code
      input)))

;;make a break point
(defmacro break []
  `(main/repl
     :prompt #(print "debug=> ")
     :read readr
     :eval (partial contextual-eval (local-context))))


(defn parse-long
  "Parses the given String to a long. If it cannot be parsed, returns nil."
  [s]
  (try
    (Long/parseLong s)
    (catch Exception _)))


(defn parse-binary
  [s]
  (try
    (Long/parseLong s 2)
    (catch Exception _)))


(defn pow
  [^long a ^long b]
  (loop [r 1
         n b]
    (if (= n 0)
      r
      (recur (* r a) (dec n)))))


(defn abs
  [x]
  (if (< x 0) (- x) x))


(def ^:private hex-lookup
  (let [hex-chars [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]]
    (->> hex-chars
         (mapcat #(interleave (repeat %) hex-chars))
         (partition 2)
         (mapv #(apply str %)))))


(defn byte->hex
  [b]
  (hex-lookup (Byte/toUnsignedInt b)))


(defn bytes->hex
  [bytes]
  (apply str (mapv byte->hex bytes)))


(defn cpmap
  "A chunked variant of pmap. Instead of using one thread for application of f,
  uses one thread for n applications of f."
  [n f col]
  (->> (partition-all n col)
       (pmap #(mapv f %))
       (apply concat)))


(defn update!
  "Same as 'update' but for transient maps"
  ([m k f]
   (assoc! m k (f (get m k))))
  ([m k f x]
   (assoc! m k (f (get m k) x)))
  ([m k f x y]
   (assoc! m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc! m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc! m k (apply f (get m k) x y z more))))


(defn permutations
  [col]
  (lazy-seq
    (if (next col)
      (apply concat
             (for [x col]
               (->> (remove #{x} col)
                    permutations
                    (map #(cons x %)))))
      [col])))


;; predicate combiners

(defn and-fn
  "Returns a function that combines all given predicates via 'and'.
  So it will only return true, if all predicates returned true for a given input."
  [& preds]
  (fn [& args]
    (every? #(apply % args) preds)))

(defn or-fn
  "Returns a function that combines all given predicates via 'or'.
  So it will return true only if at least one predicate returned true for a given input."
  [& preds]
  (fn [& args]
    (boolean (some #(apply % args) preds))))

(def not-fn
  "Takes a predicate and inverts it. (just an alias for 'clojure.core/complement')"
  complement)

(defn if-fn
  "Combines 3 predicates to a branching predicate, like an if-then-else"
  [p-test p-then p-else]
  (fn [& args]
    (if (apply p-test args)
      (apply p-then args)
      (apply p-else args))))



(defn parse-debug-value
  [[_ sym-name entries]]
  (if (nil? entries)
    (symbol sym-name)
    (let [entries (mapv
                    #(list 'quote %)
                    (edn/read-string
                      (if (string/starts-with? entries "[")
                        entries
                        (str "[" entries "]"))))]
      `(get-in ~(symbol sym-name) ~entries))))

(defmacro debug
  "println with automatically resolving placeholders.
  {x} - Resolves to the current binding of x.
  {x k} - Resolves the key 'k' in the associative datastructure x.
          The key is taken literally. To check for a key in form of
          a keyword, symbol, or string, use :k, k, or \"k\" respectively.
  {x [a b]} - Similar to {x k}, but resolves via 'get-in'."
  [^String s]
  (let [placeholders (re-seq #"\{([^{} ,]+)(?: ([^{} ,]+|\[(?:[^{} ,]+[, ]*)+\]))?\}" s)
        format-string (reduce #(string/replace-first %1 (first %2) "%s") s placeholders)
        values (->> placeholders
                    (map parse-debug-value)
                    (map #(list 'clojure.core/pr-str %)))]
    `(println (format ~format-string ~@values))))



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
     (spit inputs-file (string/trim-newline input))
     (println "Downloaded input to" (.getPath inputs-file)))
   ;; create clojure namespace file
   (let [ns-file (io/file (str "./src/clojure/aoc_" year "/day_" day ".clj"))]
     (-> ns-file .getParentFile .mkdirs)
     (when (.createNewFile ns-file)
       (-> (slurp-resource "template_ns.edn")
           (string/replace #"%>.+?<%" {"%>year<%" (str year)
                                       "%>day<%"  (str day)})
           (#(spit ns-file %)))
       (println "Created Clojure namespace in " (.getPath ns-file))))))

(comment
  (start-day)
  )