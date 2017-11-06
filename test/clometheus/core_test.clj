(ns clometheus.core-test
  (:require [clojure.test :refer :all]
            [clometheus.core :refer :all]))

(deftest counters-test
  (clear! default-registry)
  (let [my-counter (counter "my-counter" "this is my counter")]
    (testing "counter start at zero"
      (is (= 0.0 @my-counter)))
    (testing "incrementing by one works"
      (inc! my-counter)
      (is (= 1.0 @my-counter)))
    (testing "incrementing by more than one works"
      (inc! (counter "my-counter" "hello") 2)
      (is (= 3.0 @my-counter)))
    (testing "counters my only go up"
      (is (thrown? IllegalArgumentException (inc! my-counter -1))))
    (testing "already registered counters are returned and not new created"
      (is (= my-counter (counter "my-counter" "hello"))))
    (testing "labels are possible"
      (let [my-label-counter (counter "with-labels" "a counter with labels" ["my-label"])]
        (inc! (get my-label-counter {:my-label :my-val}))))
    (testing "counters are collectable"
      (is (= '(#clometheus.core.Sample{:name "my-counter", :description "this is my counter", :type :counter, :label->values {}, :value 3.0}
               #clometheus.core.Sample{:name "with-labels", :description "a counter with labels", :type :counter, :label->values {:my-label :my-val}, :value 1.0}) (collect default-registry))))))

(deftest gauge-test
  (clear! default-registry)

  (let [my-gauge (gauge "labelless-gauge" "Gauge without labels")]
    (testing "gauges start at zero"
      (is (= 0.0 @my-gauge)))
    (testing "incrementing by one works"
      (inc! my-gauge)
      (is (= 1.0 @my-gauge)))
    (testing "incrementing by more than one works"
      (inc! my-gauge 2)
      (is (= 3.0 @my-gauge)))
    (testing "already registered gauges are returned and not new created"
      (is (= my-gauge (gauge "labelless-gauge" "hello"))))
    (testing "labels are possible"
      (let [my-label-gauge (gauge "with-labels" "a gauge with labels" ["my-label"])]
        (inc! (get my-label-gauge {:my-label :my-val}))))
    (testing "gauges are collectable"
      (is (= '(#clometheus.core.Sample{:description   "a gauge with labels"
                                       :label->values {:my-label :my-val}
                                       :name          "with-labels"
                                       :type          :gauge
                                       :value         1.0}
                #clometheus.core.Sample{:description   "Gauge without labels"
                                        :label->values {}
                                        :name          "labelless-gauge"
                                        :type          :gauge
                                        :value         3.0}) (collect default-registry))))))

(deftest histogram-test
  (clear! default-registry)
  (let [my-histogram (histogram "my-histogram" :description "labeless histogram" :buckets [0.1 1 10] )]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= [0.0 0.0 0.0] @my-histogram)))
    (testing "histograms can observe values"
      (observe! my-histogram 2)
      (is (= [0.0 0.0 1.0] @my-histogram)))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (histogram "my-histogram" "labeless histogram" [0.1 1 10]))))))




