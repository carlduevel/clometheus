(ns clometheus.txt-format-test
  (:require [clojure.test :refer :all]
            [clometheus.txt-format :as f]
            [clometheus.core :as c])
  (:import (java.io StringWriter)))

(defn clear-default-registry [f]
  (c/clear! c/default-registry)
  (f))

(use-fixtures :each clear-default-registry)


(deftest labelless-metrics-are-formatted
  (let [gauge    (doto (c/gauge "labelless_gauge" :description "Gauge without labels") (c/set! 1))
        actual   (-> (StringWriter.) (f/write (.sample (c/fetch c/default-registry "labelless_gauge" :gauge)))
                     (.toString))
        expected "# HELP labelless_gauge Gauge without labels\n# TYPE labelless_gauge gauge\nlabelless_gauge 1.0\n"]
    (is (= actual expected))))

(deftest labels-are-formatted
  (let [counter  (doto (c/counter "my_counter" :description "blabla" :with-labels ["rc"])
                   (c/inc! :with-labels {"rc" 200})
                   (c/inc! :with-labels {"rc" 500} :by 3))
        actual   (-> (StringWriter.) (f/write (.sample counter)) (.toString))
        expected (str "# HELP my_counter blabla\n"
                      "# TYPE my_counter counter\n"
                      "my_counter {rc=\"500\",} 3.0\n"
                      "my_counter {rc=\"200\",} 1.0\n")]
    (is (= actual expected))))

(deftest description-and-label-values-are-escaped
  (let [counter  (doto (c/counter "my_counter" :description "\n I am weird \\" :with-labels ["foo"])
                   (c/inc! :with-labels {"foo" "\nrc\\\""}))
        actual   (-> (StringWriter.) (f/write (.sample counter)) (.toString))

        expected (str "# HELP my_counter \\n I am weird \\\\\n"
                      "# TYPE my_counter counter\n"
                      "my_counter {foo=\"\\nrc\\\\\\\"\",} 1.0\n")]
    (is (= actual expected))))

(deftest infinity-is-handled
  (let [gauge    (doto (c/gauge "gauge_with_inf" :description "Gauge without labels") (c/set! Double/POSITIVE_INFINITY))
        actual   (-> (StringWriter.) (f/write (.sample (c/fetch c/default-registry "gauge_with_inf" :gauge)))
                     (.toString))
        expected "# HELP gauge_with_inf Gauge without labels\n# TYPE gauge_with_inf gauge\ngauge_with_inf +Inf\n"]
    (is (= actual expected))))

(deftest histograms-are-handled
  (let [h      (c/histogram "my_histogram" :with-labels ["foo"] :buckets [1 2])
        _      (c/observe! h 1 :with-labels {"foo" "bar"})
        actual (-> (StringWriter.) (f/write (.sample h)) (.toString))
        expected (str "# HELP my_histogram \n"
                      "# TYPE my_histogram histogram\n"
                      "my_histogram {foo=\"bar\",le=\"1\",} 1.0\n"
                      "my_histogram {foo=\"bar\",le=\"2\",} 1.0\n"
                      "my_histogram {le=\"+Inf\",foo=\"bar\",} 1.0\n"
                      "my_histogram_count 1.0\n"
                      "my_histogram_sum 1.0\n")]
    (is (= expected actual))))

