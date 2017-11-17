(ns clometheus.core-test
  (:require [clojure.test :refer :all]
            [clometheus.core :refer :all])
  (:import (clometheus.core Counter Collector)))

(deftest counters-wo-labels-test
  (clear! default-registry)
  (let [my-counter (counter "my-counter" "this is my counter")]
    (testing "counter start at zero"
      (is (= 0.0 @my-counter)))
    (testing "incrementing by one works"
      (inc! my-counter)
      (is (= 1.0 @my-counter)))
    (testing "incrementing by more than one works"
      (inc! (counter "my-counter" "hello") :by 2)
      (is (= 3.0 @my-counter)))
    (testing "counters may only go up"
      (is (thrown? IllegalArgumentException (inc! my-counter :by -1))))
    (testing "already registered counters are returned and not new created"
      (is (= my-counter (counter "my-counter" "hello"))))
    (testing "counters are collectable"
      (is (= '(#clometheus.core.Sample{:name "my-counter" :description "this is my counter" :type :counter :label->values {{} 3.0}})
             (collect default-registry))))))

(deftest counters-with-labels-test
  (clear! default-registry)
  (let [my-counter (counter "my-counter" "blabla" ["rc"])]
    (testing "Labels are managed by Collectors"
      (is (= Collector (type my-counter))))
    (testing "Incrementing by more than one works"
      (inc! my-counter :by 2 :with-labels {:rc 500})
      (is (== 2 @(get my-counter {:rc 500}))))
    (testing "Incrementing by just one works"
      (inc! my-counter :with-labels {:rc 500})
      (is (== 3 @(get my-counter {:rc 500}))))
    (testing "Counters may only go up"
      (is (thrown? IllegalArgumentException
                   (inc! my-counter :with-labels {:rc 500} :by -1))))
    (testing "Each label combination is counted by itself"
      (inc! my-counter :with-labels {:rc 200})
      (is (== 1 @(get my-counter {:rc 200}))))
    (testing "counters are collectable"
      (is (= '(#clometheus.core.Sample{:name "my-counter" :description "blabla" :type :counter :label->values {{:rc 200} 1.0 {:rc 500} 3.0}})
             (collect default-registry))))))



(deftest gauge-test
  (clear! default-registry)
  (let [my-gauge (gauge "labelless-gauge" "Gauge without labels")]
    (testing "gauges start at zero"
      (is (= 0.0 @my-gauge)))
    (testing "incrementing by one works"
      (inc! my-gauge)
      (is (= 1.0 @my-gauge)))
    (testing "incrementing by more than one works"
      (inc! my-gauge :by 2)
      (is (= 3.0 @my-gauge)))
    (testing "already registered gauges are returned and not new created"
      (is (= my-gauge (gauge "labelless-gauge" "hello"))))
    (testing "labels are possible"
      (let [my-label-gauge (gauge "with-labels" "a gauge with labels" ["my-label"])]
        (inc! my-label-gauge :with-labels {:my-label :my-val})))
    (testing "gauges are collectable"
      (is (= '(#clometheus.core.Sample{:description   "a gauge with labels"
                                       :label->values {{:my-label :my-val} 1.0}
                                       :name          "with-labels"
                                       :type          :gauge}
                #clometheus.core.Sample{:description   "Gauge without labels"
                                        :label->values {{} 3.0}
                                        :name          "labelless-gauge"
                                        :type          :gauge}) (collect default-registry))))))

(deftest labeless-histogram-test
  (clear! default-registry)
  (let [my-histogram (histogram "my-histogram" :description "labeless histogram" :buckets [0.1 1 10])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= [0.0 0.0 0.0] @my-histogram)))
    (testing "histograms can observe values"
      (observe! my-histogram 2)
      (is (= [0.0 0.0 1.0] @my-histogram)))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (histogram "my-histogram" "labeless histogram" [0.1 1 10]))))
    (testing "histograms are collectable"
      (is (= '(#clometheus.core.Sample{:name "my-histogram", :description nil, :type :histogram, :label->values {{} (0.0 0.0 1.0)}})) (collect default-registry)))))

(deftest histogram-with-test
  (clear! default-registry)
  (let [my-histogram (histogram "histogram-with-labels" :description "histogram with labels" :buckets [0.1 1 10] :with-labels ["test"])]
    (testing "histogram exists"
      (is (not= nil my-histogram)))
    (testing "histograms start with all buckets set to zero"
      (is (= [0.0 0.0 0.0] @(get my-histogram {:test :test}))))
    (testing "histograms can observe values"
      (observe! my-histogram 2 :with-labels {:test :best}))
    (testing "already registered histograms are returned and not new created"
      (is (= my-histogram (histogram "histogram-with-labels" :description "histogram with labels" :buckets [0.1 1 10] :with-labels ["test"]))))
    (testing "histograms are collectable"
      (is (= (list (->Sample "histogram-with-labels" "histogram with labels" :histogram {{:test :best} '(0.0 0.0 1.0) {:test :test} '(0.0 0.0 0.0)})) (collect default-registry)))))

  )