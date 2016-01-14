(defproject people-names "0.1.0-SNAPSHOT"
  :description "Exercise in parsing and processing text in Clojure"
  :url "https://github.com/gpstathis/people-names-clj"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot people-names.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
