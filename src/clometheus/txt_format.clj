(ns clometheus.txt-format
  (:require [clometheus.core :as c])
  (:import (java.io Writer StringWriter)
           (clometheus.core Sample ICollectorRegistry)))


(defn- writeEscaped [^Writer writer ^String description]
  (doseq [c description]
    (case c
      \\ (.append writer "\\\\")
      \newline (.append writer "\\n")
      (.append writer c))))

(defn- writeValue [^Writer writer ^String value]
  (doseq [c value]
    (case c
      \\ (.append writer "\\\\")
      \" (.append writer "\\\"")
      \newline (.append writer "\\n")
      (.append writer c))))

(defn- write-description! [^Writer writer ^String name ^String description]
  (doto writer
    (.write "# HELP ")
    (.write name)
    (.write " "))
  (writeEscaped writer description)
  (.write writer "\n"))

(defn- write-type! [^Writer writer ^String name ^String type]
  (doto writer
    (.write "# TYPE ")
    (.write name)
    (.write " ")
    (.write type)
    (.write "\n")))

(defn- go-str [^Double d]
  (cond
    (= d Double/POSITIVE_INFINITY) "+Inf"
    (= d Double/NEGATIVE_INFINITY) "-Inf"
    (Double/isNaN d) "NaN"
    :else (Double/toString d)))

(defn- write-values! [^Writer writer ^String metric-name labels->values]
  (doseq [[k v] labels->values]
    (if (empty? k)
      (.write writer (str metric-name " " (go-str v) "\n"))
      (do
        (.write writer (str metric-name " {"))
        (doseq [[label label-value] k]
          (.write writer (name label))
          (.write writer "=\"")
          (writeValue writer (str label-value))
          (.write writer "\","))
        (.write writer "} ")
        (.write writer (go-str v))
        (.write writer "\n")))))

(defn write [^Writer writer ^Sample {:keys [id description type label->values total-sum total-count]}]
  (write-description! writer id description)
  (write-type! writer id (name type))
  (write-values! writer id label->values)
  (when (and total-sum total-count)
    (.write writer (format "%s_count %s\n" id (go-str total-count)))
    (.write writer (format "%s_sum %s\n" id (go-str total-sum))))
  writer)

(defn metrics-response [^ICollectorRegistry registry]
  {:headers {"Content-Type" "text/plain; version=0.0.4; charset=utf-8"}
   :status 200
   :body    (let [writer (StringWriter.)]
              (doseq [sample (c/collect registry)]
                (write writer sample))
              (.toString writer))})