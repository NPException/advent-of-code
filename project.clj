(defproject advent-of-code "0.1.0-SNAPSHOT"
  :license {:name "MIT License"
            :url "https://en.wikipedia.org/wiki/MIT_License"}
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns aoc-utils}
  :dependencies [[org.clojure/clojure "1.12.3"]
                 [org.clojure/core.match "1.1.0"]
                 [http-kit "2.8.1"]
                 [hickory "0.7.1"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.7"]
                 [criterium "0.4.6"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
