(defproject online.duevel/clometheus "0.2.2-SNAPSHOT"
  :description "Prometheus client library"
  :url "https://github.com/carlduevel/clometheus"
  :license {:name "Apache License 2.0"
            :url  "https://www.apache.org/licenses/LICENSE-2.0"}
  :min-lein-version "2.0.0"
  :lein-release {:deploy-via :clojars}
  :java-source-paths ["java"]
  :javac-options     ["-target" "1.6" "-source" "1.6"]
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :profiles {:dev {:plugins [[lein-release/lein-release "1.0.9"]]}})
