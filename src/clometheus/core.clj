(ns clometheus.core
  (:import (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic DoubleAdder)
           (clojure.lang ILookup Keyword IDeref)
           (java.io Writer)))

(defprotocol ICollectorRegistry
  (fetch [this name])
  (register-or-return! [this collector])
  (clear! [this])
  (collect [this]))


(defprotocol ICollector
  (name [this])
  (description [this])
  (metric-type [this])
  (samples [this]))

(extend-type ConcurrentHashMap
  ICollectorRegistry
  (fetch [this name]
    (.get this name))                                       ; type needs to be checked
  (register-or-return! [this collector]
    (let [name (.name collector)]
      (if-let [found-collector (.get this name)]
        found-collector
        (or (.putIfAbsent this (.name collector) collector) collector))))
  (clear! [this] (.clear this))
  (collect [this]
    (flatten (map samples (.values this)))))


(def default-registry (ConcurrentHashMap.))

(defrecord Sample [^String name ^String description ^Keyword type label->values ^Double value])

(deftype Collector [^String name ^String description ^Keyword type ^ConcurrentHashMap label-values->collectors labels metric-fn]
  ICollector
  (name [_this] name)
  (description [_this] description)
  (metric-type [this] type)
  (samples [_this]
    (for [[label-value collector] label-values->collectors]
      (->Sample name description type label-value @collector)))
  ILookup
  (valAt [this key]
    (if-let [found-metric (.get label-values->collectors key)]
      found-metric
      (let [new-metric         (metric-fn)
            current-val-or-nil (.putIfAbsent label-values->collectors key new-metric)]
        (or current-val-or-nil new-metric))))
  (valAt [this key notfound]
    (throw (UnsupportedOperationException. "Sorry"))))

;alter-meta to make it private
; must always be the same set of metrics - exception when one is not set?


(defn fetch-or-create-collector! [name description labels type metric-fn]
  (if-let [collector (fetch default-registry name)]
    collector
    (register-or-return! default-registry (->Collector name description type (ConcurrentHashMap.) labels metric-fn))))

(defprotocol Incrementable
  (inc! [this] [this ^Double value]))

(defrecord Counter [^DoubleAdder current-val]
  IDeref
  (deref [_this] (.sum current-val))
  Incrementable
  (inc! [this]
    (inc! this 1))
  (inc! [_this increment]
    (if (pos? increment)
      (.add current-val increment)
      (throw (IllegalArgumentException. "Counters cannot be incremented with negative values")))))

(defn counter
  ([name description]
   (let [collector (counter name description [])]
     (get collector {})))
  ([name description labels]
   (fetch-or-create-collector! name description labels :counter #(Counter. (DoubleAdder.)))))

(defprotocol Decrementable
  (dec! [this] [this value]))

(defprotocol Resettable
  (set! [this value]))

(defrecord Gauge [^DoubleAdder current-val]
  IDeref
  (deref [_this] (.sum current-val))
  Incrementable
  (inc! [this] (inc! this 1))
  (inc! [_this val] (.add current-val val))
  Decrementable
  (dec! [this] (dec! this 1))
  (dec! [_this val] (.add current-val (* -1 val)))
  Resettable
  (set! [_this current-val] (locking current-val (.reset current-val) (.add current-val current-val))))

(defn gauge
  ([name description]
   (let [collector (gauge name description [])]
     (get collector {})))
  ([name description labels]
   (fetch-or-create-collector! name description labels :gauge #(Gauge. (DoubleAdder.)))))

(defprotocol Observable
  (observe! [this value]))

(defrecord Histogram [bucket-sizes bucket-adders cumulative-counts]
  IDeref
  (deref [_this] (map #(.sum %) bucket-adders))
  Observable
  (observe! [this value]
    (doseq [[size bucket] (map list bucket-sizes bucket-adders)] (when (<= value size) (.add bucket 1)))
    (.add cumulative-counts 1)))

;TODO: Default for description


(defn- create-histogram! [buckets]
  (->Histogram buckets (for [i (range (count buckets))] (DoubleAdder.)) (DoubleAdder.)))

(defn histogram [name & {description :descrition buckets :buckets labels :labels :or {:buckets [0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1, 2.5, 5, 7.5, 10] :labels [] :description ""}}]
  (let [collector (fetch-or-create-collector! name description labels :histogram (partial create-histogram! buckets))]
    (if (empty? labels)
      (get collector {})
      collector)))



(defmethod print-method Histogram [h ^Writer writer]
  (.write writer (str "Histogram named" (.-bucket_sizes h))))


;
;(defprotocol Histogram
;  (observe! [this ^Double value]))



