(ns web-utils
  ; Namespace copied and modified from hcscraper project
  (:require [hickory.core :as hickory]
            [clojure.java.io :as io]
            [org.httpkit.client :as http])
  (:import (java.util Map)))

(def tag
  "returns the tag keyword of the hiccup element"
  first)

(def attribs
  "returns the attribute map of the hiccup element"
  second)

(def body
  "returns the children/body of the hiccup element"
  nnext)


(defn ^:private submap?
  "Checks whether m contains all entries in sub."
  [^Map sub ^Map m case-sensitive?]
  (loop [ks (keys sub)]
    (or (nil? ks)
        (let [a (get sub (first ks) ::NOT_FOUND)
              b (get m (first ks) ::NOT_FOUND)]
          (if (or (= a b)
                  (and (not case-sensitive?)
                       (string? a)
                       (string? b)
                       (= (.toLowerCase ^String a) (.toLowerCase ^String b))))
            (recur (next ks))
            false)))))


(defn ^:private search-all*
  "Searches a hiccup element hierarchy for elements with the given tag and matching attributes"
  [element target-tag? literal-attribs predicates-match? & [case-sensitive?]]
  (when (vector? element)
    (let [match? (and (or (nil? target-tag?) (target-tag? (tag element)))
                      (submap? literal-attribs (attribs element) case-sensitive?)
                      (or (nil? predicates-match?)
                          (predicates-match? (attribs element))))]
      (loop [children (body element)
             results  (if match? [element] [])]
        (if (empty? children)
          results
          (recur (rest children)
            (into results (search-all* (first children) target-tag? literal-attribs predicates-match? case-sensitive?))))))))


(defn search-all
  "Searches a hiccup element hierarchy for elements with the given tag and matching attributes.
  `target-tag` can either be a keyword for literal tag matches, or a function which will receive tag keywords.
  `target-attribs` may contain literal strings as values for literal matches, and functions for finer grained searches."
  [element target-tag target-attribs & [case-sensitive?]]
  (let [target-tag?       (when target-tag
                            (if (keyword? target-tag)
                              (fn [tag] (= tag target-tag))
                              target-tag))
        literal?          (comp string? val)
        literal-attribs   (into {} (filter literal?) target-attribs)
        predicate-attribs (into {} (filter (complement literal?)) target-attribs)
        predicates-match? (when-not (empty? predicate-attribs)
                            (fn [attribs]
                              (every?
                                (fn [[k pred]]
                                  (pred (attribs k)))
                                predicate-attribs)))]
    (search-all* element target-tag? literal-attribs predicates-match? case-sensitive?)))


(defn search
  "Searches a hiccup element hierarchy for the first element with the given tag and matching attributes"
  [element target-tag target-attribs & [case-sensitive?]]
  (first (search-all element target-tag target-attribs case-sensitive?)))


(defn parse-html
  "Parses the given HTML string to a hiccup data structure."
  [html]
  (->> html
       hickory/parse
       hickory/as-hiccup
       (filter vector?)
       first))


(defn load-url [url & {:as options}]
  (some->
    (http/request (merge {:method :get :url url} options))
    deref
    :body))


(defn load-hiccup [url & {:as options}]
  (some->
    url
    (load-url options)
    parse-html))


(defn download
  "Downloads data from the given URL, and outputs it to f via spit."
  [url f]
  (println "Start downloading" f " - "
    (try
      (-> @(http/request {:method :head :url url}) :headers :content-length
          Long/parseLong
          (quot 1000))
      (catch Exception _
        "unknown"))
    "kB")
  (with-open [in  (io/input-stream url)
              out (io/output-stream f)]
    (io/copy in out))
  (println "Finished downloading" f)
  (flush)
  true)
