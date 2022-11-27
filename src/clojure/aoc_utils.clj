(ns aoc-utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.main :as main]
            [clojure.math :as math]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [web-utils :as web])
  (:import (clojure.lang IPersistentVector)
           (de.npe.utils LongBox)
           (java.time LocalDateTime)
           (java.util Arrays Comparator HashMap HashSet PriorityQueue)
           (java.util.function ToDoubleFunction)))


(defn slurp-resource
  [path]
  (slurp (io/resource path)))


(defn read-as-vector
  "Surrounds the given string with [ and ], then reads it as edn."
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


(def ^:private hex-lookup
  "A vector of all 256 hex values for a byte:
  [\"00\" \"01\" \"02\" ... \"fd\" \"fe\" \"ff\"]"
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
          byte-num     (alength bytes)
          sb           (StringBuilder. ^long (* 2 byte-num))]
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


(defn rpartial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with additional args + args.
  So compared to `partial`, it passes the partialed args last."
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f x arg1))
     ([x y] (f x y arg1))
     ([x y z] (f x y z arg1))
     ([x y z & args] (apply f x y z (concat args [arg1])))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f x arg1 arg2))
     ([x y] (f x y arg1 arg2))
     ([x y z] (f x y z arg1 arg2))
     ([x y z & args] (apply f x y z (concat args [arg1 arg2])))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f x arg1 arg2 arg3))
     ([x y] (f x y arg1 arg2 arg3))
     ([x y z] (f x y z arg1 arg2 arg3))
     ([x y z & args] (apply f x y z (concat args [arg1 arg2 arg3])))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f (concat args [arg1 arg2 arg3] more)))))


(defmacro nth-in
  "Macro to do highly efficient lookup in nested vectors."
  ([v is]
   `(-> ~v ~@(map (fn [i] (list `nth i)) is)))
  ([v is not-found]
   `(-> ~v
        ~@(map (fn [i] (list `nth i nil)) (butlast is))
        ~(list `nth (last is) not-found))))


(defn cpmap
  "A chunked variant of pmap. Instead of using one thread for application of f,
  uses one thread for n applications of f."
  [n f col]
  (->> (partition-all n col)
       (pmap #(mapv f %))
       (apply concat)))


(def ^:private keywordizing
  "Transducer to keywordize the keys in map entries"
  (map (fn [[k v :as entry]]
         (if (or (string? k) (symbol? k))
           [(keyword k) v]
           entry))))

(defn keywordize-keys
  "Recursively transforms all map keys from strings/symbols to keywords.
  (copied and modified from `clojure.walk/keywordize-keys`)"
  [m]
  ;; only apply to maps
  (walk/postwalk
    (fn [x]
      (if (map? x)
        (into {} keywordizing x)
        x))
    m))


(defn first-match
  "Returns the first x in coll for which (pred x) returns logical true, else nil"
  [pred coll]
  (when-let [[x & remain] (seq coll)]
    (if (pred x) x (recur pred remain))))


(defn index-of
  "Returns the index of the first element in coll which matches pred"
  [pred coll]
  (loop [coll coll
         i    0]
    (when-let [[e & more] (seq coll)]
      (if (pred e) i (recur more (inc i))))))


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
  kf on each element. The value at each key will be a vector of the
  corresponding elements mapped by mf, in the order they appeared in coll."
  [kf mf coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (kf x)]
          (assoc! ret k (conj (get ret k []) (mf x)))))
      (transient {}) coll)))


(defn permutations
  "Returns a lazy sequence of all possible
  rearrangements for a collection of unique elements."
  [col]
  (let [vcol    (vec col)
        indices ((fn iperms [indices]
                   (lazy-seq
                     (if (next indices)
                       (apply concat
                         (for [x indices]
                           (->> (remove #(= % x) indices)
                                iperms
                                (map #(cons x %)))))
                       [indices])))
                 (range (count vcol)))]
    (map #(mapv vcol %) indices)))


(defn combinations
  "Generate a list of all possible n-sized tuple combinations in coll."
  [n coll]
  (let [coll     (vec coll)
        size     (count coll)
        comb-aux (fn comb-aux
                   [^long m ^long start]
                   (if (= 1 m)
                     (for [x (range start size)]
                       (list (nth coll x)))
                     (for [^long x (range start size)
                           xs      (comb-aux (dec m) (inc x))]
                       (cons (nth coll x) xs))))]
    (comb-aux n 0)))


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


(defn find-all-divisors
  "Returns a vector of all possible divisors of n in ascending order."
  [^long n]
  (let [low-divs         (->> (range 1 (inc (long (math/sqrt n))))
                              (filterv #(zero? (rem n ^long %))))
        skip-first-high? (let [^long i (peek low-divs)]
                           (= i (quot n i)))]
    (into low-divs
      (comp
        (if skip-first-high? (drop 1) identity)
        (map #(quot n ^long %)))
      (rseq low-divs))))


(defn vpartition
  "Returns a lazy sequence of vectors of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap.
  Uses subvec to increase partitioning performance. Note that this means
  the source vector will not be GC'ed while any of the partitions are alive."
  ;; TODO: implement 4-arity variant with pad collection
  ([n ^IPersistentVector v]
   (vpartition n n v))
  ([^long n ^long step ^IPersistentVector v]
   (lazy-seq
     (let [num (count v)]
       (when (>= num n)
         (cons (subvec v 0 n)
           (vpartition n step (subvec v step num))))))))


(defn vpartition-all
  "Returns a lazy sequence of vectors like partition, but may include partitions with fewer than n items at the end.
  Uses subvec to increase partitioning performance. Note that this means
  the source vector will not be GC'ed while any of the partitions are alive."
  ([n ^IPersistentVector v]
   (vpartition-all n n v))
  ([^long n ^long step ^IPersistentVector v]
   (lazy-seq
     (let [num (count v)]
       (if (<= num n)
         [v]
         (cons (subvec v 0 n)
           (vpartition-all n step (subvec v step num))))))))


(defn partitioning
  "A transducer variation of clojure.core/partition."
  ([^long n] (partitioning n n))
  ([^long n ^long step]
   ;; blank partition only used to efficiently clear partition elements on finish to allow GC
   (let [blank-partition     (object-array n)
         last-index          (dec n)
         step-diff           (- n step)
         needs-partial-copy? (pos? step-diff)]
     (fn [rf]
       (let [partition (object-array n)
             length    (LongBox. 0)]
         (fn
           ([] (rf))
           ([result]
            ;; allow GC to collect elements of the last partition, even if transducer is kept around
            (System/arraycopy blank-partition 0 partition 0 n)
            (rf result))
           ([result input]
            (let [current-length (.get length)]
              (when-not (neg? current-length)
                (aset partition current-length input))
              (.set length (inc current-length))
              (if (= current-length last-index)
                (let [v (vec (Arrays/copyOf partition n))]
                  (when needs-partial-copy?
                    (System/arraycopy partition step partition 0 step-diff))
                  (.set length step-diff)
                  (rf result v))
                result)))))))))


(defn count-matching
  "Counts the number of elements in coll which match the given predicate."
  ^long [pred coll]
  (reduce
    (fn [^long r v] (if (pred v) (inc r) r))
    0
    coll))


(defn distinctv
  "Like `distinct`, but eager. The transducer variant is a bit more efficient
  than `distinct` because it uses a Java HashSet under the hood."
  ([]
   (fn [rf]
     (let [seen (HashSet.)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (.add seen input)
            (rf result input)
            result))))))
  ([coll]
   (let [seen (HashSet.)]
     (persistent!
       (reduce
         (fn [acc v]
           (if (.add seen v)
             (conj! acc v)
             acc))
         (transient [])
         coll)))))


(defn rows->columns
  "Takes a sequence of same-sized rows (a grid) and returns a vector of the columns instead. (eager)"
  [rows]
  (->> (apply interleave rows)
       (partition (count rows))
       (mapv vec)))


(defn A*-search
  "A* implementation translated from https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
  nil elements are not permitted. (might implement later)
  Paramters:
  `start` - The state from which the search should begin.
  `goal?` - Predicate function which takes a node and must return true if the given value represents the desired final state.
  `neighbours-fn` - Given a state, has to return a sequence of states that the search can continue traversing.
  `heuristic-fn` - Given a state, should return a cost estimate of going from the state to the goal.
                   (Bad estimates can make the search really slow. When unsure, try `(constantly 0)` as fallback.
  `cost-fn` - Given the current state and a neighbor state, must return the cost of moving to the neighbor state."
  [start goal? neighbours-fn heuristic-fn cost-fn]
  (let [came-from  (HashMap.)                               ;; For node n, (came-from n) is the node immediately preceding it on the cheapest path from start to n currently known.
        g-score    (HashMap.)                               ;; <double> For node n, (g-score n) is the cost of the cheapest path from start to n currently known. (default: infinity)
        f-score    (HashMap.)                               ;; <double> For node n, (f-score n) is (g-score n) + (heuristic-fn n). (f-score n) represents our current best guess as to how short a path from start to finish can be if it goes through n.
        open-set   (HashSet.)                               ;; The set of discovered nodes that may need to be (re-)expanded. Initially, only the start node is known.
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
        (if (goal? current)
          ;; construct result
          (loop [n    current
                 path (list current)]
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
  ([pred] pred)
  ([pred1 pred2]
   (fn
     ([] (and (pred1) (pred2)))
     ([a] (and (pred1 a) (pred2 a)))
     ([a b] (and (pred1 a b) (pred2 a b)))
     ([a b c] (and (pred1 a b c) (pred2 a b c)))
     ([a b c & more] (and (apply pred1 a b c more)
                          (apply pred2 a b c more)))))
  ([pred1 pred2 & preds]
   (and-fn pred1 (apply and-fn pred2 preds))))

(defn or-fn
  "Returns a function that combines all given predicates via 'or'.
  So it will return true only if at least one predicate returned true for a given input."
  ([pred] pred)
  ([pred1 pred2]
   (fn
     ([] (or (pred1) (pred2)))
     ([a] (or (pred1 a) (pred2 a)))
     ([a b] (or (pred1 a b) (pred2 a b)))
     ([a b c] (or (pred1 a b c) (pred2 a b c)))
     ([a b c & more] (or (apply pred1 a b c more)
                         (apply pred2 a b c more)))))
  ([pred1 pred2 & preds]
   (and-fn pred1 (apply and-fn pred2 preds))))

(defn not-fn
  "Takes a predicate and inverts it. (equivalent to 'clojure.core/complement', with additional 3-arity body)"
  [pred]
  (fn
    ([] (not (pred)))
    ([x] (not (pred x)))
    ([x y] (not (pred x y)))
    ([x y z] (not (pred x y z)))
    ([x y z & more] (not (apply pred x y z more)))))

(defn if-fn
  "Combines 3 predicates to a branching predicate, like an if-then-else"
  [p-test p-then p-else]
  (fn
    ([] (if (p-test) (p-then) (p-else)))
    ([a] (if (p-test a) (p-then a) (p-else a)))
    ([a b] (if (p-test a b) (p-then a b) (p-else a b)))
    ([a b c] (if (p-test a b c) (p-then a b c) (p-else a b c)))
    ([a b c & more] (if (apply p-test a b c more)
                      (apply p-then a b c more)
                      (apply p-else a b c more)))))

;; logical IMPLY (single arrow): rule is true unless `p-test` (p) is true and `p-then` (q) is false. ("We care about q only if p is true.")
(defn imply-fn
  "Combines 2 predicates. If the first one returns true, the second one must also return true."
  [p-pred q-pred]
  (or-fn (not-fn p-pred) q-pred))

;; logical XNOR (double arrow): returns true only if p and q have the same result.
(defn iff-fn
  "Combines 2 predicates. The resulting predicate returns true when both predicates return the same result."
  [p-pred q-pred]
  (fn
    ([] (= (p-pred) (q-pred)))
    ([a] (= (p-pred a) (q-pred a)))
    ([a b] (= (p-pred a b) (q-pred a b)))
    ([a b c] (= (p-pred a b c) (q-pred a b c)))
    ([a b c & more] (= (apply p-pred a b c more)
                       (apply q-pred a b c more)))))



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
  (let [placeholders  (re-seq #"\{([^{} ,]+)(?: ([^{} ,]+|\[(?:[^{} ,]+[, ]*)+\]))?\}" s)
        format-string (reduce #(str/replace-first %1 (first %2) "%s") s placeholders)
        values        (->> placeholders
                           (map parse-debug-value)
                           (map #(list 'clojure.core/pr-str %)))]
    `(println (format ~format-string ~@values))))



(def ^:dynamic *aoc-session-id* nil)

(defn load-session-id []
  (if-some [session-id (or *aoc-session-id*
                           (System/getenv "AOC_SESSION")
                           (try
                             (str/trim (slurp "session-id.txt"))
                             (catch Exception _)))]
    session-id
    (throw (IllegalStateException. "No AOC session id present. You have 3 options to specify one:
             Bind it to `aoc-utils/*aoc-session-id*` via `binding`,
             set it as an environment variable `AOC_SESSION`,
             or store it in a file named `session-id.txt` in the working directory."))))


(defn start-day
  "Initializes a new namespace for the given day and downloads the input for the day.
  This requires a valid session id in the environment variable 'AOC_SESSION'."
  ([]
   (let [now (LocalDateTime/now)]
     (start-day (-> now .getYear) (-> now .getDayOfMonth))))
  ([year day]
   (let [headers     {:headers {"cookie" (str "session=" (load-session-id))}}
         inputs-file (io/file (str "./resources/inputs/aoc_" year "/day-" day ".txt"))
         ns-file     (io/file (str "./src/clojure/aoc_" year "/day_" day ".clj"))
         input       (web/load-url (str "https://adventofcode.com/" year "/day/" day "/input") headers)
         task-page   (web/load-hiccup (str "https://adventofcode.com/" year "/day/" day) headers)
         task-title  (-> (web/search task-page :h2 nil)
                         (web/body)
                         first)]
     ;; create input text file
     (-> inputs-file .getParentFile .mkdirs)
     (when (.createNewFile inputs-file)
       (spit inputs-file (str/trim-newline input))
       (println "Downloaded input to" (.getPath inputs-file)))
     ;; create clojure namespace file
     (-> ns-file .getParentFile .mkdirs)
     (when (.createNewFile ns-file)
       (-> (slurp-resource "template_ns.edn")
           (str/replace #"%>.+?<%" {"%>year<%"  (str year)
                                    "%>day<%"   (str day)
                                    "%>title<%" task-title})
           (#(spit ns-file %)))
       (println "Created Clojure namespace in " (.getPath ns-file)))
     ;; print easter egg(s)
     (when-some [eggs (seq (web/search-all task-page :span {:title some?}))]
       (println "Easter egg mouse-overs:")
       (doseq [egg-element eggs]
         (println " " (first (web/body egg-element))))))))


(defn start-next-unsolved-day
  "Looks for the first day which doesn't have a namespace, and calls `start-day` for it."
  []
  (->> (range)
       (map #(+ 2015 %))
       (mapcat (fn [year]
                 (map #(vector year %) (range 1 26))))
       (first-match
         (fn [[year day]]
           (not (.exists (io/file (str "./src/clojure/aoc_" year "/day_" day ".clj"))))))
       (apply start-day)))


(comment
  (start-day)
  (start-next-unsolved-day)
  )