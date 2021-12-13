(ns aoc-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.main :as main]
            [org.httpkit.client :as http]
            [clojure.walk :as walk])
  (:import (java.time LocalDateTime)
           (java.util ArrayDeque)))


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


(defn parse-binary
  ^long [s]
  (try
    (Long/parseLong s 2)
    (catch Exception _)))


(defn pow
  ^long [^long a ^long b]
  (loop [r 1
         n b]
    (if (= n 0)
      r
      (recur (* r a) (dec n)))))


(defn abs
  ^long [^long x]
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
       (apply concat)
       (vec)))


(defn keywordize-keys
  "Recursively transforms all map keys from strings/symbols to keywords.
  (copied and modified from `clojure.walk/keywordize-keys`)"
  [m]
  (let [f (fn [[k v]]
            (if (or (string? k) (symbol? k))
              [(keyword k) v]
              [k v]))]
    ;; only apply to maps
    (walk/postwalk
      (fn [x]
        (if (map? x)
          (into {} (map f x))
          x))
      m)))


(defn first-match
  "Returns the first x in coll for which (pred x) returns logical true, else nil"
  [pred coll]
  (when-let [[x & remain] (seq coll)]
    (if (pred x) x (recur pred remain))))


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


(defn group-by-and-map
  "Returns a map of the elements of coll keyed by the result of
  gf on each element. The value at each key will be a vector of the
  corresponding elements mapped by mf, in the order they appeared in coll."
  {:added "1.2"
   :static true}
  [gf mf coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (gf x)]
          (assoc! ret k (conj (get ret k []) (mf x)))))
      (transient {}) coll)))


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


(defn partitions
  "Generates a sequence of all possible options for
  how the number n can be split up into k numbers"
  [^long n ^long k]
  (if (> k 1)
    (mapcat (fn [^long x]
              (map (partial cons x)
                   (partitions (- n x) (dec k))))
            (range 1 (inc (- n (dec k)))))
    [[n]]))


(defn partition-xf
  "A transducer variation of clojure.core/partition."
  ([^long n] (partition-xf n n))
  ([^long n ^long step]
   (fn [rf]
     (let [a (ArrayDeque. n)]
       (fn
         ([] (rf))
         ([result]
          (let [v (when (= (.size a) n)
                    (vec (.toArray a)))
                ;; allow early garbage collection
                _ (.clear a)
                result (if v
                         (unreduced (rf result v))
                         result)]
            (rf result)))
         ([result input]
          (.add a input)
          (if (= n (.size a))
            (let [v (vec (.toArray a))]
              (dotimes [_ step]
                (.removeFirst a))
              (rf result v))
            result)))))))


(defn count-matching
  "Counts the number of elements in coll which match the given predicate."
  ^long [pred coll]
  (reduce
    (fn [^long r v] (if (pred v) (inc r) r))
    0
    coll))


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