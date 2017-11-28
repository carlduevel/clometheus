(ns clometheus.core-test
  (:require [clojure.test :refer :all]
            [clometheus.core :as c])
  (:import (clometheus.core Counter Collector)
           (java.io StringWriter)))

(defn clear-default-registry [f]
  (c/clear! c/default-registry)
  (f))

(use-fixtures :each clear-default-registry)

(deftest counters-wo-labels-test
  (let [my-counter (c/counter "my_counter" :description "this is my counter")]
    (testing "counter start at zero"
      (is (= 0.0 @my-counter)))
    (testing "incrementing by one works"
      (c/inc! my-counter)
      (is (= 1.0 @my-counter)))
    (testing "incrementing by more than one works"
      (c/inc! (c/counter "my_counter" :description "hello") :by 2)
      (is (= 3.0 @my-counter)))
    (testing "counters may only go up"
      (is (thrown? IllegalArgumentException (c/inc! my-counter :by -1))))
    (testing "already registered counters are returned and not new created"
      (is (= my-counter (c/counter "my_counter"))))
    (testing "counters are collectable"
      (is (= [(c/map->Sample {:name "my_counter" :description "this is my counter" :type :counter :label->values {{} 3.0}})]
             (c/collect c/default-registry))))
    (testing "counters can be registered to other registries than the default one"
      (is (not= my-counter (c/counter "my_counter" :registry (c/registry)))))))

(deftest counters-with-labels-test
  (let [my-counter (c/counter "my_counter" :description "blabla" :with-labels ["rc"])]
    (testing "Labels are managed by Collectors"
      (is (= Collector (type my-counter))))
    (testing "Incrementing by more than one works"
      (c/inc! my-counter :by 2 :with-labels {:rc 500})
      (is (== 2 @(get my-counter {:rc 500}))))
    (testing "Incrementing by just one works"
      (c/inc! my-counter :with-labels {:rc 500})
      (is (== 3 @(get my-counter {:rc 500}))))
    (testing "Counters may only go up"
      (is (thrown? IllegalArgumentException
                   (c/inc! my-counter :with-labels {:rc 500} :by -1))))
    (testing "Each label combination is counted by itself"
      (c/inc! my-counter :with-labels {:rc 200})
      (is (== 1 @(get my-counter {:rc 200}))))
    (testing "counters are collectable"
      (is (= [(c/map->Sample {:name          "my_counter"
                              :description   "blabla"
                              :type          :counter
                              :label->values {{:rc 200} 1.0 {:rc 500} 3.0}})]
             (c/collect c/default-registry))))))

(deftest simple-gauge-test
  (let [my-gauge (c/gauge "labelless_gauge" :description "Gauge without labels")]
    (testing "gauges start at zero"
      (is (= 0.0 @my-gauge)))
    (testing "incrementing by one works"
      (c/inc! my-gauge)
      (is (= 1.0 @my-gauge)))
    (testing "incrementing by more than one works"
      (c/inc! my-gauge :by 2)
      (is (= 3.0 @my-gauge)))
    (testing "already registered gauges are returned and not new created"
      (is (= my-gauge (c/gauge "labelless_gauge" :description "hello"))))
    (testing "Setting gauges to a specific value is possible"
      (c/set! my-gauge 1.0)
      (is (= 1.0 @my-gauge)))
    (testing "gauges are collectable"
      (is (= [(c/map->Sample {:description   "Gauge without labels"
                              :label->values {{} 1.0}
                              :name          "labelless_gauge"
                              :type          :gauge})] (c/collect c/default-registry))))
    (testing "gauges can be registered to other registries than the default one"
      (is (not= my-gauge (c/gauge "labelless_gauge" :registry (c/registry)))))))

(deftest gauge-with-labels-test
  (testing "labels are possible"
    (let [my-label-gauge (c/gauge "with_labels" :description "a gauge with labels" :with-labels ["my_label"])]
      (c/inc! my-label-gauge :with-labels {:my_label :my-val})
      (c/set! my-label-gauge 2.0 :with-labels {:my_label :my-val}))
    (is (= [(c/map->Sample {:description   "a gauge with labels"
                            :label->values {{:my_label :my-val} 2.0}
                            :name          "with_labels"
                            :type          :gauge})]
           (c/collect c/default-registry)))))

(deftest labeless-histogram-test
  (let [my-histogram (c/histogram "my_histogram")]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= (repeat 14 0.0) @my-histogram)))
    (testing "histograms can observe values"
      (c/observe! my-histogram 2)
      (is (= (concat (repeat 10 1.0) (repeat 4 0.0)) @my-histogram)))
    (testing "histograms have a total count"
      (is (= 1 (:count my-histogram))))
    (testing "histograms have a total sum"
      (is (= 2(:sum my-histogram))))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (c/histogram "my_histogram" :description "labeless histogram" :buckets [0.1 1 10]))))
    (testing "histograms are collectable"
      (is (= [(c/map->Sample {:name          "my_histogram"
                              :description   nil
                              :type          :histogram
                              :label->values {{} [0.0 0.0 1.0]}})])
          (c/collect c/default-registry)))
    (testing "histograms can be registered to other registries than the default one"
      (is (not= my-histogram (c/histogram "my_histogram" :registry (c/registry)))))))

(deftest histogram-with-labels-test
  (let [my-histogram (c/histogram "histogram_with_labels" :description "histogram with labels" :buckets [0.1 1 10] :with-labels ["test"])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= [0.0 0.0 0.0] @(get my-histogram {:test :test}))))
    (testing "histograms can observe values"
      (c/observe! my-histogram 2 :with-labels {:test :best}))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (c/histogram "histogram_with_labels" :description "histogram with labels" :buckets [0.1 1 10] :with-labels ["test"]))))
    (testing "histograms are collectable"
      (is (= [(c/->Sample "histogram_with_labels" "histogram with labels" :histogram {{:test :best} '(1.0 1.0 0.0) {:test :test} '(0.0 0.0 0.0)})]
             (c/collect c/default-registry))))
    (testing "histograms cannot have 'le' as a label name"
      (is (thrown-with-msg? IllegalArgumentException #"'le' is a reserved label name for buckets." (c/histogram "le_as_label_name_is_reserved" :with-labels ["le"]))))))

(defn str-represenation [of]
  (let [w (StringWriter.)]
    (print-method of w)
    (.toString w)))

(deftest all-abstractions-should-be-printable
  (is (= "#clometheus.core.Gauge{:current-val 0.0}" (str-represenation (c/gauge "gauge"))))
  (is (= "#clometheus.core.Counter{:current-val 0.0}" (str-represenation (c/counter "counter"))))
  (is (= "#clometheus.core.Histogram{:bucket-sizes (0.1 1 10), :bucket-adders (0.0 0.0 0.0), :count 0.0, :sum 0.0}" (str-represenation (c/histogram "my_histogram" :description "labeless histogram" :buckets [0.1 1 10])))))

(deftest restriction-on-metric-names-test
  (testing "Special chars are not allowed in a metric name."
    (is (thrown-with-msg? IllegalArgumentException #"Invalid metric name:" (c/gauge "%!=!"))))
  (testing "Label names must not contain dashes."
    (is (thrown-with-msg? IllegalArgumentException #"Invalid label name:" (c/gauge "legal_name" :with-labels ["illlegal-label"]))))
  (testing "Label names starting with two dashes are reserved for internal use."
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Invalid label name: '__internal_label'.\n Label names beginning with two underscores are reserved for internal use."
          (c/gauge "legal_name" :with-labels ["__internal_label"]))))
  (testing "Two different metrics cannot be registered under the same name."
    (is (thrown-with-msg? IllegalArgumentException
                          #"Metric my_metric is a counter and not a gauge"
                          (c/counter "my_metric")
                          (c/gauge "my_metric")))))

