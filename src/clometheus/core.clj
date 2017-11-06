(ns clometheus.core
  (:import (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic DoubleAdder)
           (clojure.lang ILookup Keyword IDeref)))

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
    (.get this name))                        ; type needs to be checked
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

(defmulti new-metric
          (fn [type] type))



(deftype Collector [^String name ^String description ^Keyword type ^ConcurrentHashMap label-values->collectors labels]
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
      (let [new-metric         (new-metric type)
            current-val-or-nil (.putIfAbsent label-values->collectors key new-metric)]
        (or current-val-or-nil new-metric))))
  (valAt [this key notfound]
    (throw (UnsupportedOperationException. "Sorry"))))

;alter-meta to make it private
; must always be the same set of metrics - exception when one is not set?


(defn register-collector [name description labels type]
  (if-let [collector (fetch default-registry name)]
    collector
    (register-or-return! default-registry (->Collector name description type (ConcurrentHashMap.) labels))))

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
   (register-collector name description labels :counter)))



(defmethod new-metric :counter
  [_type]
  (Counter. (DoubleAdder.)))


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

(defmethod new-metric :gauge [_type]
  (Gauge. (DoubleAdder.)))

(defn gauge
  ([name description]
   (let [collector (counter name description [])]
     (get collector {})))
  ([name description labels]
   (register-collector name description labels :gauge)))


;
;(defprotocol Histogram
;  (observe! [this ^Double value]))



