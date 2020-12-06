(defproject advent-of-code "0.1.0-SNAPSHOT"
  :license {:name "MIT License"
            :url "https://en.wikipedia.org/wiki/MIT_License"}
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns aoc-utils}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [http-kit "2.5.0"]
                 [criterium "0.4.6"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
