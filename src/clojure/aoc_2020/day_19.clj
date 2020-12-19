(ns aoc-2020.day-19
  (:require [clojure.string :as string]
            [aoc-utils :as u]))

;; --- Day 19: Monster Messages --- https://adventofcode.com/2020/day/19

(def task-input (u/slurp-resource "inputs/aoc_2020/day-19.txt"))

(def test-input "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")


(defn parse-rule
  [rules rule-string]
  (let [[id condition] (string/split rule-string #": ")]
    [id
     (delay
       (if (string/starts-with? condition "\"")
         (subs condition 1 (dec (count condition)))
         (str "(?:"
              (-> condition
                  (string/replace #"\d+" #(deref (@rules %)))
                  (string/replace " " ""))
              ")")))]))


(defn parse-rules
  [rules-input]
  (let [rules (promise)]
    (->> (string/split-lines rules-input)
         (map (partial parse-rule rules))
         (into {})
         (deliver rules))
    (reduce-kv #(assoc %1 %2 @%3) {} @rules)))


(defn parse-input
  [input]
  (let [[rules messages] (string/split input #"\n\n")]
    [(parse-rules rules)
     (string/split-lines messages)]))


;; part 1

(defn part-1
  [input]
  (let [[rules messages] (parse-input input)
        rule-0-regex (re-pattern (rules "0"))]
    (->> messages
         (filter #(re-matches rule-0-regex %))
         count)))


;; part 2

(defn cut-31
  [pattern-31-end message]
  (loop [s message
         n 0]
    (let [remaining (string/replace-first s pattern-31-end "")]
      (if (= remaining s)
        (when (> n 0) [n s])
        (recur
          remaining
          (inc n))))))

(defn cut-42
  [pattern-42-end n partial-message]
  (loop [s partial-message
         n n]
    (if (= n 0)
      s
      (let [remaining (string/replace-first s pattern-42-end "")]
        (when (not= remaining s)
          (recur remaining (dec n)))))))

(defn find-string-before-11-match
  "Returns the part of the string before rule 11 is matched, or nil if no match is found"
  [pattern-31-end pattern-42-end message]
  (some->> message
           (cut-31 pattern-31-end)
           (apply cut-42 pattern-42-end)))


(defn part-2
  [input]
  (let [[rules messages] (parse-input input)
        regex-31 (rules "31")
        regex-42 (rules "42")
        before-11 (partial find-string-before-11-match
                           (re-pattern (str regex-31 \$))
                           (re-pattern (str regex-42 \$)))
        pattern-8 (re-pattern (str regex-42 \+))
        matches-rule-0? #(when-let [sub (before-11 %)]
                           (re-matches pattern-8 sub))]
    (count (filter matches-rule-0? messages))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 3
  (part-1 task-input)                                       ; => 241

  ;; Part 2
  (part-2 test-input)                                       ; => 12
  (part-2 task-input)                                       ; => 424

  )
