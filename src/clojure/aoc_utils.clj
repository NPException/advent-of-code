(ns aoc-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.main :as main]
            [org.httpkit.client :as http]
            [clojure.walk :as walk])
  (:import (java.time LocalDateTime)
           (java.util ArrayDeque HashMap HashSet PriorityQueue Comparator)
           (java.util.function ToDoubleFunction)))


(defn slurp-resource
  [path]
  (slurp (io/resource path)))


(defn read-as-vector
  [input]
  (read-string (str "[" input "]")))


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
  ^String [b]
  (hex-lookup (Byte/toUnsignedInt b)))


(defn bytes->hex
  ^String [bytes]
  (if (bytes? bytes)
    ;; fast path
    (let [^bytes bytes bytes
          byte-num (alength bytes)
          sb (StringBuilder. ^long (* 2 byte-num))]
      (loop [i 0]
        (when (< i byte-num)
          (.append sb (byte->hex (aget bytes i)))
          (recur (inc i))))
      (.toString sb))
    ;; generic impl
    (let [sb (StringBuilder.)]
      (loop [[b & bytes] bytes]
        (if b
          (do (.append sb (byte->hex b))
              (recur bytes))
          (.toString sb))))))


(defn cpmap
  "A chunked variant of pmap. Instead of using one thread for application of f,
  uses one thread for n applications of f."
  [n f col]
  (->> (partition-all n col)
       (pmap #(mapv f %))
       (apply concat)))


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


(defn sum-int-range
  "Returns the sum of integers in the given range."
  [^long from ^long to]
  (/ (* (inc (- to from))
        (+ from to))
     2))


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


(defn A*-search
  "A* implementation translated from https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
  nil elements are not permitted. (might implement later)"
  [start goal neighbours-fn heuristic-fn cost-fn]
  (let [came-from (HashMap.)                                ;; For node n, (came-from n) is the node immediately preceding it on the cheapest path from start to n currently known.
        g-score (HashMap.)                                  ;; <double> For node n, (g-score n) is the cost of the cheapest path from start to n currently known. (default: infinity)
        f-score (HashMap.)                                  ;; <double> For node n, (f-score n) is (g-score n) + (h n). (f-score n) represents our current best guess as to how short a path from start to finish can be if it goes through n.
        open-set (HashSet.)                                 ;; The set of discovered nodes that may need to be (re-)expanded. Initially, only the start node is known.
        open-queue (PriorityQueue.
                     (Comparator/comparingDouble
                       (reify ToDoubleFunction
                         (applyAsDouble [_ e] (.doubleValue ^Number (.getOrDefault f-score e Double/POSITIVE_INFINITY))))))]
    (.put g-score start 0.0)
    (.put f-score start (.doubleValue ^Number (heuristic-fn start)))
    (.add open-set start)
    (.add open-queue start)
    (loop []
      (when-some [current (.poll open-queue)]
        (.remove open-set current)
        (if (= current goal)
          ;; construct result
          (loop [n goal
                 path (list goal)]
            (if-some [n (.get came-from n)]
              (recur n (conj path n))
              path))
          ;; continue search
          (do (doseq [neighbour (neighbours-fn current)]
                ;; (cost-fn current neighbor) is the weight of the edge from current to neighbor
                ;; tentative-g-score is the distance from start to the neighbor through current
                (let [tentative-g-score (+ ^double (.getOrDefault g-score current Double/POSITIVE_INFINITY)
                                           (.doubleValue ^Number (cost-fn current neighbour)))]
                  (when (< tentative-g-score ^double (.getOrDefault g-score neighbour Double/POSITIVE_INFINITY))
                    ;; This path to neighbor is better than any previous one. Record it!
                    (.put came-from neighbour current)
                    (.put g-score neighbour tentative-g-score)
                    (.put f-score neighbour (+ tentative-g-score (.doubleValue ^Number (heuristic-fn neighbour))))
                    (when-not (.contains open-set neighbour)
                      (.add open-queue neighbour)
                      (.add open-set neighbour)))))
              (recur)))))))


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
                      (if (str/starts-with? entries "[")
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
        format-string (reduce #(str/replace-first %1 (first %2) "%s") s placeholders)
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
     (spit inputs-file (str/trim-newline input))
     (println "Downloaded input to" (.getPath inputs-file)))
   ;; create clojure namespace file
   (let [ns-file (io/file (str "./src/clojure/aoc_" year "/day_" day ".clj"))]
     (-> ns-file .getParentFile .mkdirs)
     (when (.createNewFile ns-file)
       (-> (slurp-resource "template_ns.edn")
           (str/replace #"%>.+?<%" {"%>year<%" (str year)
                                       "%>day<%"  (str day)})
           (#(spit ns-file %)))
       (println "Created Clojure namespace in " (.getPath ns-file))))))

(comment
  (start-day)
  )