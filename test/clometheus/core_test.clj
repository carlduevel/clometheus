(ns clometheus.core-test
  (:require [clojure.test :refer :all]
            [clometheus.core :as c])
  (:import (clometheus.core Collector Histogram Summary)
           (java.io StringWriter)))

(defn clear-default-registry [f]
  (c/clear! c/default-registry)
  (f))

(use-fixtures :each clear-default-registry)

(deftest labelless-counter-test
  (let [my-counter (c/counter "my_counter" :description "this is my counter")]
    (testing "counter start at zero"
      (is (== 0 @my-counter)))
    (testing "incrementing by one works"
      (c/inc! my-counter)
      (is (== 1 @my-counter)))
    (testing "incrementing by more than one works"
      (c/inc! (c/counter "my_counter" :description "hello") :by 2)
      (is (== 3 @my-counter)))
    (testing "counters may only go up"
      (is (thrown? IllegalArgumentException (c/inc! my-counter :by -1))))
    (testing "already registered counters are returned and not new created"
      (is (= my-counter (c/counter "my_counter"))))
    (testing "counters are collectable"
      (is (= [(c/map->Sample {:id "my_counter" :description "this is my counter" :type :counter :label->values {{} 3.0}})]
             (c/collect c/default-registry))))
    (testing "counters can be registered to other registries than the default one"
      (is (not= my-counter (c/counter "my_counter" :registry (c/registry)))))))

(deftest counter-labels-test
  (let [my-counter (c/counter "my_counter" :description "blabla" :labels ["rc"])]
    (testing "Labels are managed by Collectors"
      (is (= Collector (type my-counter))))
    (testing "Incrementing by more than one works"
      (c/inc! my-counter :by 2 :labels {"rc" 500})
      (is (== 2 @(c/get-or-create-metric! my-counter {"rc" 500}))))
    (testing "Incrementing by just one works"
      (c/inc! my-counter :labels {"rc" 500})
      (is (== 3 @(c/get-or-create-metric! my-counter {"rc" 500}))))
    (testing "Counters may only go up"
      (is (thrown? IllegalArgumentException
                   (c/inc! my-counter :labels {"rc" 500} :by -1))))
    (testing "Labels have to be complete"
      (is (thrown-with-msg? IllegalArgumentException #"Wrong or insufficient labels provided."
                            (c/inc! my-counter))))
    (testing "counters do not have a total sum"
      (is (= nil (.sum my-counter))))
    (testing "counters do not have an update counter"
      (is (= nil (.updates my-counter))))

    (testing "Each label combination is counted by itself"
      (c/inc! my-counter :labels {"rc" 200})
      (is (== 1 @(c/get-or-create-metric! my-counter {"rc" 200}))))
    (testing "counters are collectable"
      (is (= [(c/map->Sample {:id            "my_counter"
                              :description   "blabla"
                              :type          :counter
                              :label->values {{"rc" 200} 1.0 {"rc" 500} 3.0}})]
             (c/collect c/default-registry))))))

(deftest labelless-gauge-test
  (let [my-gauge (c/gauge "labelless_gauge" :description "Gauge without labels")]
    (testing "gauges start at zero"
      (is (== 0 @my-gauge)))
    (testing "incrementing by one works"
      (c/inc! my-gauge)
      (is (== 1 @my-gauge)))
    (testing "incrementing by more than one works"
      (c/inc! my-gauge :by 2)
      (is (== 3 @my-gauge)))
    (testing "already registered gauges are returned and not new created"
      (is (= my-gauge (c/gauge "labelless_gauge" :description "hello"))))
    (testing "Setting gauges to a specific value is possible"
      (c/set! my-gauge 1.0)
      (is (== 1 @my-gauge)))
    (testing "gauges are collectable"
      (is (= [(c/map->Sample {:description   "Gauge without labels"
                              :label->values {{} 1.0}
                              :id            "labelless_gauge"
                              :type          :gauge})] (c/collect c/default-registry))))
    (testing "gauges can be registered to other registries than the default one"
      (is (not= my-gauge (c/gauge "labelless_gauge" :registry (c/registry)))))))

(deftest gauge-labels-test
  (let [my-label-gauge (c/gauge "with_labels" :description "a gauge with labels" :labels ["my_label"])]
    (testing "labels are possible"
      (c/set! my-label-gauge 2.0 :labels {"my_label" "my_val"})
      (c/inc! my-label-gauge :labels {"my_label" "my_val"})
      (c/dec! my-label-gauge :labels {"my_label" "my_val"})
      (is (= [(c/map->Sample {:description   "a gauge with labels"
                              :label->values {{"my_label" "my_val"} 2.0}
                              :id            "with_labels"
                              :type          :gauge})]
             (c/collect c/default-registry))))
    (testing "Labels have to be complete"
      (is (thrown-with-msg? IllegalArgumentException #"Wrong or insufficient labels provided."
                            (c/inc! my-label-gauge)))
      (is (thrown-with-msg? IllegalArgumentException #"Wrong or insufficient labels provided."
                            (c/set! my-label-gauge 3)))
      (is (thrown-with-msg? IllegalArgumentException #"Wrong or insufficient labels provided."
                            (c/dec! my-label-gauge))))))

(deftest callback-gauge-test
  (let [callback-gauge (c/gauge "my_callback_gauge" :callback-fn (constantly 3.0))]
    (testing "Callback gauges call a passed function on deref"
      (is (= 3.0 @callback-gauge)))
    (testing
      (is (= [(c/map->Sample {:id            "my_callback_gauge"
                              :description   ""
                              :type          :gauge
                              :label->values {{} 3.0}})]
             (c/collect c/default-registry))))
    (testing "Callback gauge cannot be incremented."
      (is (thrown? IllegalArgumentException (c/inc! callback-gauge))))
    (testing "Callback gauges cannot be decremented"
      (is (thrown? IllegalArgumentException (c/dec! callback-gauge))))
    (testing "Callback gauges cannot be set"
      (is (thrown? IllegalArgumentException (c/set! callback-gauge 5))))
    (testing "labels are not supported  on callback gauges"
      (is (thrown? IllegalArgumentException (c/gauge "callback_with_labels" :labels ["not" "possible"] :callback-fn (constantly 1)))))))


(deftest labeless-histogram-test
  (let [^Histogram my-histogram (c/histogram "my_histogram" :buckets [1.0 2.0 3.0])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= {{:le "1.0"} 0.0 {:le "2.0"} 0.0 {:le "3.0"} 0.0 {:le "+Inf"} 0.0})) @my-histogram)
    (testing "histograms can observe values"
      (c/observe! my-histogram 2)
      (is (= {{:le "1.0"} 0.0 {:le "2.0"} 1.0 {:le "3.0"} 1.0 {:le "+Inf"} 1.0} @my-histogram)))
    (testing "histograms have a total count of updates"
      (is (== 1 (.updates my-histogram))))
    (testing "histograms have a total sum"
      (is (== 2 (.sum my-histogram))))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (c/histogram "my_histogram"))))
    (testing "histograms are collectable"
      (is (= [(c/map->Sample {:id            "my_histogram"
                              :description   ""
                              :type          :histogram
                              :label->values {{:le "1.0"}  0.0
                                              {:le "2.0"}  1.0
                                              {:le "3.0"}  1.0
                                              {:le "+Inf"} 1.0}
                              :total-count   1.0
                              :total-sum     2.0})]
             (c/collect c/default-registry))))
    (testing "histograms can be registered to other registries than the default one"
      (is (not= my-histogram (c/histogram "my_histogram" :registry (c/registry)))))))

(deftest histogram-labels-test
  (let [my-histogram (c/histogram "histogram_with_labels" :description "histogram with labels" :buckets [0.1 1 10] :labels ["test"])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= {{:le "0.1"} 0.0, {:le "1"} 0.0, {:le "10"} 0.0, {:le "+Inf"} 0.0} @(c/get-or-create-metric! my-histogram {"test" "test"}))))
    (testing "histograms can observe values"
      (c/observe! my-histogram 2 :labels {"test" "best"}))
    (testing "histograms have a total count of updates"
      (is (== 1 (.updates my-histogram))))
    (testing "histograms have a total sum"
      (is (== 2 (.sum my-histogram))))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (c/histogram "histogram_with_labels" :description "histogram with labels" :buckets [0.1 1 10] :labels ["test"]))))
    (testing "histograms are collectable"
      (is (= [(c/map->Sample
                {:id            "histogram_with_labels"
                 :description   "histogram with labels"
                 :type          :histogram
                 :label->values {{:le "0.1" "test" "test"}  0.0
                                 {:le "1" "test" "test"}    0.0
                                 {:le "10" "test" "test"}   0.0
                                 {:le "+Inf" "test" "test"} 0.0
                                 {:le "0.1" "test" "best"}  0.0
                                 {:le "1" "test" "best"}    0.0
                                 {:le "10" "test" "best"}   1.0
                                 {:le "+Inf" "test" "best"} 1.0}
                 :total-count   1.0
                 :total-sum     2.0})]
             (c/collect c/default-registry))))
    (testing "Labels have to be complete"
      (is (thrown-with-msg? IllegalArgumentException #"Wrong or insufficient labels provided."
                            (c/observe! my-histogram 1))))
    (testing "histograms cannot have 'le' as a label name"
      (is (thrown-with-msg? IllegalArgumentException #"'le' is a reserved label for histograms." (c/histogram "le_as_label_name_is_reserved" :labels ["le"]))))))

(deftest labeless-summary-wo-quantiles-test
  (testing "max-age-seconds must be positive"
    (is (thrown-with-msg? IllegalArgumentException #"Max-age-seconds must be positive\." (c/summary "foo" :max-age-seconds 0))))
  (testing "age-buckets must be positive"
    (is (thrown-with-msg? IllegalArgumentException #"Age-buckets must be positive\." (c/summary "foo" :age-buckets 0))))
  (let [^Summary my-summary (c/summary "my_summary_wo_quantiles")]
    (testing "summary exists"
      (is (not= nil my-summary)))
    (testing "summaries have a total count of updates that start at zero."
      (is (== 0.0 (.updates my-summary))))
    (testing "summaries have a total sum that start as zero"
      (is (== 0.0 (.sum my-summary))))
    (testing "already registered summaries are returned and not new created"
      (is (= my-summary (c/summary "my_summary_wo_quantiles"))))
    (testing "summaries are collectable"
      (is (= [(c/map->Sample
                {:id            "my_summary_wo_quantiles"
                 :description   ""
                 :type          :summary
                 :label->values {}
                 :total-count   0.0
                 :total-sum     0.0})]
             (c/collect c/default-registry))))
    (testing "summarys can be registered to other registries than the default one"
      (is (not= my-summary (c/summary "my_summary_wo_quantiles" :registry (c/registry)))))))


(deftest labeless-summary-with-quantiles-test
  (let [^Summary my-summary (c/summary "my_summary_with_quantiles" :quantiles [(c/quantile 0.99 0.3) (c/quantile 0.90 0.3) (c/quantile 0.75 0.3) (c/quantile 0.5 0.3)])]
    (testing "summary exists"
      (is (not= nil my-summary)))
    (testing "summaries can observe values"
      (c/observe! my-summary 2)
      (is (= {{:quantile "0.5"}  2.0
              {:quantile "0.75"} 2.0
              {:quantile "0.9"}  2.0
              {:quantile "0.99"} 2.0} @my-summary)))
    (testing "summaries have a total count of updates"
      (is (== 1 (.updates my-summary))))
    (testing "summaries have a total sum"
      (is (== 2 (.sum my-summary))))
    (testing "already registered summaries are returned and not new created"
      (is (= my-summary (c/summary "my_summary_with_quantiles"))))
    (testing "summaries are collectable"
      (is (= [(c/map->Sample
                {:id            "my_summary_with_quantiles"
                 :description   ""
                 :type          :summary
                 :label->values {{:quantile "0.5"}  2.0
                                 {:quantile "0.75"} 2.0
                                 {:quantile "0.9"}  2.0
                                 {:quantile "0.99"} 2.0}
                 :total-count   1.0
                 :total-sum     2.0})]
             (c/collect c/default-registry))))
    (testing "summarys can be registered to other registries than the default one"
      (is (not= my-summary (c/summary "my_summary_with_quantiles" :registry (c/registry)))))))

(deftest summary-with-quantiles-test
  (testing "quantile is not a valid label for summaries"
    (is (thrown-with-msg? IllegalArgumentException #"'quantile' is a reserved label for summaries." (c/summary "foo" :labels ["bar" "quantile"]))))
  (let [^Summary my-summary (c/summary "my_summary_with_quantiles_and_labels" :quantiles [(c/quantile 0.75 0.3) (c/quantile 0.99 0.3)] :labels ["status"])]
    (testing "summary exists"
      (is (not= nil my-summary)))
    (testing "summaries can observe values"
      (c/observe! my-summary 2 :labels {"status" "ok"}))
    (testing "already registered summaries are returned and not new created"
      (is (= my-summary (c/summary "my_summary_with_quantiles_and_labels" :labels ["status"]))))
    (testing "summaries are collectable"
      (is (= [(c/map->Sample {:description   ""
                              :id            "my_summary_with_quantiles_and_labels"
                              :label->values {{"status"  "ok"
                                               :quantile "0.75"} 2.0
                                              {"status"  "ok"
                                               :quantile "0.99"} 2.0}
                              :total-count   1.0
                              :total-sum     2.0
                              :type          :summary})]
             (c/collect c/default-registry))))
    (testing "summarys can be registered to other registries than the default one"
      (is (not= my-summary (c/summary "my_summary_with_quantiles_and_labels" :registry (c/registry)))))))



(defn str-represenation [of]
  (let [w (StringWriter.)]
    (print-method of w)
    (.toString w)))

(deftest all-abstractions-should-be-printable
  (is (= "#clometheus.core.Gauge{:current-val 0.0}" (str-represenation (c/gauge "gauge"))))
  (is (= "#clometheus.core.Counter{:current-val 0.0}" (str-represenation (c/counter "counter"))))
  (is (= "#clometheus.core.Histogram{:bucket-sizes (0.1 1 10), :bucket-adders (0.0 0.0 0.0), :total-bucket 0.0, :count-and-sum #clometheus.core.CountAndSum{:counter 0.0, :summer 0.0}}" (str-represenation (c/histogram "my_histogram" :description "labeless histogram" :buckets [0.1 1 10])))))

(deftest restriction-on-metric-names-test
  (testing "Special chars are not allowed in a metric name."
    (is (thrown-with-msg? IllegalArgumentException #"Invalid metric name:" (c/gauge "%!=!"))))
  (testing "Label names must not contain dashes."
    (is (thrown-with-msg? IllegalArgumentException #"Invalid label name:" (c/gauge "legal_name" :labels ["illlegal-label"]))))
  (testing "Label names starting with two dashes are reserved for internal use."
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Invalid label name: '__internal_label'.\n Label names beginning with two underscores are reserved for internal use."
          (c/gauge "legal_name" :labels ["__internal_label"]))))
  (testing "Two different metrics cannot be registered under the same name."
    (is (thrown-with-msg? IllegalArgumentException
                          #"Metric my_metric is a counter and not a gauge"
                          (c/counter "my_metric")
                          (c/gauge "my_metric")))))

(deftest timed-test
  (testing "timing is done in seconds"
    (let [histogram (c/histogram "time_this_code" :buckets [0.001 0.01 0.1])]
      (is (= "done" (c/time histogram
                            (Thread/sleep 10)
                            "done")))
      (is (= [0.0 1.0 1.0 1.0] (vals @histogram))))))