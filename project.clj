(defproject online.duevel/clometheus "0.1.1-SNAPSHOT"
  :description "Prometheus client library"
  :url "https://github.com/hackbert/clometheus"
  :license {:name "Apache License 2.0"
            :url  "https://www.apache.org/licenses/LICENSE-2.0"}
  :lein-release {:deploy-via :clojars}
  :java-source-paths ["java"]
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:plugins [[lein-release/lein-release "1.0.9"]]}})
