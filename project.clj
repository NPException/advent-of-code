(defproject advent-of-code "0.1.0-SNAPSHOT"
  :license {:name "MIT License"
            :url "https://en.wikipedia.org/wiki/MIT_License"}
  :global-vars {*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed}
  :repl-options {:init-ns aoc-utils}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [http-kit "2.6.0"]
                 [hickory "0.7.1"]
                 [criterium "0.4.6"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
