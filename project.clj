(defproject org.clojars.ikoblik/clj-index "0.0.2"
  :description "String indexing and matching algorithms"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :plugins [[lein-swank "1.4.4"]]
  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}

  :compile-path "classes"
)
