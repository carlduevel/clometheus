(ns clometheus.core-test
  (:require [clojure.test :refer :all]
            [clometheus.core :as c])
  (:import (clometheus.core Counter Collector)))

(defn clear-default-registry [f]
  (c/clear! c/default-registry)
  (f))

(use-fixtures :each clear-default-registry)

(deftest counters-wo-labels-test
  (let [my-counter (c/counter "my-counter" "this is my counter")]
    (testing "counter start at zero"
      (is (= 0.0 @my-counter)))
    (testing "incrementing by one works"
      (c/inc! my-counter)
      (is (= 1.0 @my-counter)))
    (testing "incrementing by more than one works"
      (c/inc! (c/counter "my-counter" "hello") :by 2)
      (is (= 3.0 @my-counter)))
    (testing "counters may only go up"
      (is (thrown? IllegalArgumentException (c/inc! my-counter :by -1))))
    (testing "already registered counters are returned and not new created"
      (is (= my-counter (c/counter "my-counter" "hello"))))
    (testing "counters are collectable"
      (is (= [(c/map->Sample {:name "my-counter" :description "this is my counter" :type :counter :label->values {{} 3.0}})]
             (c/collect c/default-registry))))))

(deftest counters-with-labels-test
  (let [my-counter (c/counter "my-counter" "blabla" ["rc"])]
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
      (is (= [(c/map->Sample {:name          "my-counter"
                              :description   "blabla"
                              :type          :counter
                              :label->values {{:rc 200} 1.0 {:rc 500} 3.0}})]
             (c/collect c/default-registry))))))

(deftest simple-gauge-test
  (let [my-gauge (c/gauge "labelless-gauge" "Gauge without labels")]
    (testing "gauges start at zero"
      (is (= 0.0 @my-gauge)))
    (testing "incrementing by one works"
      (c/inc! my-gauge)
      (is (= 1.0 @my-gauge)))
    (testing "incrementing by more than one works"
      (c/inc! my-gauge :by 2)
      (is (= 3.0 @my-gauge)))
    (testing "already registered gauges are returned and not new created"
      (is (= my-gauge (c/gauge "labelless-gauge" "hello"))))
    (testing "Setting gauges to a specific value is possible"
      (c/set! my-gauge 1.0)
      (is (= 1.0 @my-gauge)))
    (testing "gauges are collectable"
      (is (= [(c/map->Sample {:description   "Gauge without labels"
                              :label->values {{} 1.0}
                              :name          "labelless-gauge"
                              :type          :gauge})] (c/collect c/default-registry))))))

(deftest gauge-with-labels-test
  (testing "labels are possible"
    (let [my-label-gauge (c/gauge "with-labels" "a gauge with labels" ["my-label"])]
      (c/inc! my-label-gauge :with-labels {:my-label :my-val})
      (c/set! my-label-gauge 2.0 :with-labels {:my-label :my-val}))
    (is (= [(c/map->Sample {:description   "a gauge with labels"
                            :label->values {{:my-label :my-val} 2.0}
                            :name          "with-labels"
                            :type          :gauge})]
           (c/collect c/default-registry)))))

(deftest labeless-histogram-test
  (let [my-histogram (c/histogram "my-histogram" :description "labeless histogram" :buckets [0.1 1 10])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= [0.0 0.0 0.0] @my-histogram)))
    (testing "histograms can observe values"
      (c/observe! my-histogram 2)
      (is (= [0.0 0.0 1.0] @my-histogram)))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (c/histogram "my-histogram" "labeless histogram" [0.1 1 10]))))
    (testing "histograms are collectable"
      (is (= [(c/map->Sample {:name          "my-histogram"
                              :description   nil
                              :type          :histogram
                              :label->values {{} [0.0 0.0 1.0]}})])
          (c/collect c/default-registry)))))

(deftest histogram-with-test
  (let [my-histogram (c/histogram "histogram-with-labels" :description "histogram with labels" :buckets [0.1 1 10] :with-labels ["test"])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= [0.0 0.0 0.0] @(get my-histogram {:test :test}))))
    (testing "histograms can observe values"
      (c/observe! my-histogram 2 :with-labels {:test :best}))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (c/histogram "histogram-with-labels" :description "histogram with labels" :buckets [0.1 1 10] :with-labels ["test"]))))
    (testing "histograms are collectable"
      (is (= [(c/->Sample "histogram-with-labels" "histogram with labels" :histogram {{:test :best} '(0.0 0.0 1.0) {:test :test} '(0.0 0.0 0.0)})]
             (c/collect c/default-registry))))))