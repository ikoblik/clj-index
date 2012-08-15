(defproject clj-index "0.0.1-SNAPSHOT"
  :description "String indexing and matching algorithms"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  ;:plugins [[lein-swank "1.4.4"]]
  :java-source-paths ["src/main/java"]
  :java-source-path "src/main/java"  

  :source-path "src/main/clojure"
  :test-path "src/test/clojure"

  :compile-path "classes"
  :prep-tasks ["javac" "compile"]
)
