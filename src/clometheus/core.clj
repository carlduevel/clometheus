(ns clometheus.core
  (:import (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic DoubleAdder)
           (clojure.lang ILookup Keyword IDeref ExceptionInfo IRecord PersistentHashSet)
           (java.io Writer)))

(defprotocol ICollectorRegistry
  (fetch [this name type])
  (register-or-return! [this name type collector-fn])
  (clear! [this])
  (collect [this]))


(defprotocol ICollector
  (id [this])
  (description [this])
  (metric-type [this])
  (sample [this])
  (get-or-create-metric! [this labels->vals]))

(def valid-metric-name-re #"[a-zA-Z_:][a-zA-Z0-9_:]*")
(defn validate-metric-name [name]
  (when-not (re-matches valid-metric-name-re name)
    (throw (IllegalArgumentException. (str "Invalid metric name: '" name
                                           "'. Metric name has to match this regex: [a-zA-Z_:][a-zA-Z0-9_:]*")))))


(let [valid-label-re    #"[a-zA-Z_][a-zA-Z0-9_]*"
      reserved-label-re #"__.*"]
  (defn validate-label-name [label-name]
    (when-not (re-matches valid-label-re label-name)
      (throw (IllegalArgumentException.
               (str "Invalid label name: '" label-name
                    "'.\n Label name has to match this regex: [a-zA-Z_][a-zA-Z0-9_]*"))))
    (when (re-matches reserved-label-re label-name)
      (throw (IllegalArgumentException.
               (str "Invalid label name: '" label-name
                    "'.\n Label names beginning with two underscores are reserved for internal use."))))))




(defn validate-label-names [label-names]
  (doseq [label label-names]
    (validate-label-name label)))

(extend-type ConcurrentHashMap
  ICollectorRegistry
  (fetch [this n t]
    (when-let [found-collector (.get this n)]
      (if (= (.metric_type found-collector) t)
        found-collector
        (throw (IllegalArgumentException.
                 (format "Metric %s is a %s and not a %s"
                         n (name (.metric_type found-collector)) (name t)))))))
  (register-or-return! [this name type collector-fn]
    (if-let [found-collector (fetch this name type)]
      found-collector
      (let [collector (collector-fn)]
        (validate-metric-name name)
        (validate-label-names (.labels collector))
        (or (.putIfAbsent this (.id collector) collector) collector))))
  (clear! [this] (.clear this))
  (collect [this]
    (map sample (.values this))))


(defn ^ICollectorRegistry registry [] (ConcurrentHashMap.))

(defonce default-registry (registry))

(defrecord Sample [^String id ^String description ^Keyword type label->values ^Double total-count ^Double total-sum])

(defprotocol ICountAndSum
  (count-and-sum! [this v]))

(defprotocol Updates
  (updates [this]))

(defprotocol Sum
  (sum [this]))

(defrecord CountAndSum [^DoubleAdder counter ^DoubleAdder summer]
  ICountAndSum
  (count-and-sum! [_this v]
    (.add counter 1.0)
    (.add summer v))
  Updates
  (updates [_this]
    (.sum counter))
  Sum
  (sum [_this]
    (.sum summer)))

(defrecord Collector [^String id ^String description ^Keyword type ^ConcurrentHashMap label-values->collectors
                      ^PersistentHashSet labels metric-fn ^CountAndSum count-and-sum]
  ICollector
  (id [_this] id)
  (description [_this] description)
  (metric-type [this] type)
  (sample [this]
    (let [labels->values (reduce-kv (fn [m labels collector]
                                      (cond
                                        (number? @collector)
                                          (assoc m labels @collector)
                                        (associative? @collector)
                                          (let [buckets->values @collector]
                                            (merge m (zipmap (for [bucket (keys buckets->values)] (merge bucket labels)) (vals buckets->values)))))) {}
                                    (into {} label-values->collectors))]

      (->Sample id description type labels->values (.updates this) (.sum this))))
  (get-or-create-metric! [this key]
    (if-let [found-metric (.get label-values->collectors key)]
      found-metric
      (let [new-metric         (metric-fn)
            current-val-or-nil (.putIfAbsent label-values->collectors key new-metric)]
        (or current-val-or-nil new-metric))))
  Updates
  (updates [_this]
    (when count-and-sum
      (.updates count-and-sum)))
  Sum
  (sum [_this]
    (when count-and-sum
      (.sum count-and-sum))))

;alter-meta to make it private
; must always be the same set of metrics - exception when one is not set?

(defprotocol Incrementable
  (increment! [this] [this ^Double value]))


(defrecord Counter [^DoubleAdder current-val]
  IDeref
  (deref [_this] (.sum current-val))
  Incrementable
  (increment! [this]
    (increment! this 1))
  (increment! [_this increment]
    (if (pos? increment)
      (.add current-val increment)
      (throw (IllegalArgumentException. "Counters cannot be incremented with negative values")))))

(defn counter-collector-fn [id description labels]
  (fn [] (map->Collector {:id                       id
                          :description              description
                          :type                     :counter
                          :label-values->collectors (ConcurrentHashMap.)
                          :labels                   (set labels)
                          :metric-fn                #(Counter. (DoubleAdder.))})))
(defn counter
  ([id & {description :description labels :with-labels registry :registry
          :or         {labels [] description "" registry default-registry}}]
   (let [collector (register-or-return! registry id :counter (counter-collector-fn id description labels))]
     (if (empty? labels)
       (get-or-create-metric! collector {})
       collector))))

(defprotocol Decrementable
  (decrement! [this] [this value]))

(defprotocol Resettable
  (reset! [this value]))

(defrecord Gauge [^DoubleAdder current-val]
  IDeref
  (deref [_this] (.sum current-val))
  Incrementable
  (increment! [this] (increment! this 1))
  (increment! [_this val] (.add current-val val))
  Decrementable
  (decrement! [this] (decrement! this 1))
  (decrement! [_this val] (.add current-val (* -1 val)))
  Resettable
  (reset! [_this new-val] (locking current-val (.reset current-val) (.add current-val new-val))))

(defn gauge-collector-fn [id description labels]
  (fn [] (map->Collector
           {:id                       id
            :description              description
            :type                     :gauge
            :label-values->collectors (ConcurrentHashMap.)
            :labels                   (set labels)
            :metric-fn                #(Gauge. (DoubleAdder.))})))

(defn gauge
  [id & {description :description labels :with-labels registry :registry
         :or         {labels [] description "" registry default-registry}}]
  (let [collector (register-or-return! registry id :gauge (gauge-collector-fn id description labels))]
    (if (empty? labels)
      (get-or-create-metric! collector {})
      collector)))

(defprotocol Observable
  (observation! [this value]))

(defn type-dispatch-one-or-more-args
  ([this]
   (type this))
  ([this & opts]
   (type this)))

(defn type-dispatch-2-or-more-args
  ([this val]
   (type this))
  ([this val & opts]
   (type this)))

(defmulti inc! type-dispatch-one-or-more-args)
(defmulti dec! type-dispatch-one-or-more-args)
(defmulti observe! type-dispatch-2-or-more-args)
(defmulti set! type-dispatch-2-or-more-args)

(defmethod inc!
  Counter
  ([this & {:keys [with-labels by] :or {with-labels {} by 1}}]
    (if (empty? with-labels)
      (increment! this by)
      (throw (Exception. "No labels are possible for simple counter!")))))

(defmethod inc!
  Gauge
  ([this & {:keys [with-labels by] :or {with-labels {} by 1}}]
    (if (empty? with-labels)
      (increment! this by)
      (throw (Exception. "No labels are possible for simple gauge!")))))

(defmethod dec!
  Gauge
  ([this & {:keys [with-labels by] :or {with-labels {} by 1}}]
    (if (empty? with-labels)
      (decrement! this by)
      (throw (Exception. "No labels are possible for simple gauge!")))))

(defmethod set!
  Gauge
  ([this val & {:keys [with-labels] :or {with-labels {}}}]
    (if (empty? with-labels)
      (reset! this val)
      (throw (Exception. "No labels are possible for simple gauge!")))))

(defn validate-labels [^Collector collector labels->values]
  (when-not (= (.labels collector) (set (keys labels->values)))
    (throw (IllegalArgumentException.
             (str "Wrong or insufficient labels provided. All and only these must be set: " (.labels collector))))))

(defmethod inc!
  Collector
  ([this & {:keys [with-labels by] :or {with-labels {} by 1}}]
    (validate-labels this with-labels)
    (increment! (get-or-create-metric! this with-labels) (or by 1))))

(defmethod dec!
  Collector
  ([this & {:keys [with-labels by] :or {with-labels {} by 1}}]
    (validate-labels this with-labels)
    (decrement! (get-or-create-metric! this with-labels) (or by 1))))

(defmethod observe!
  Collector
  ([this val & {:keys [with-labels] :or {with-labels {}}}]
    (validate-labels this with-labels)
    (observation! (get-or-create-metric! this with-labels) val)))

(defmethod set!
  Collector
  ([this val & {:keys [with-labels] :or {with-labels {}}}]
    (validate-labels this with-labels)
    (reset! (get-or-create-metric! this with-labels) val)))



(defrecord Histogram [bucket-sizes bucket-adders ^DoubleAdder total-bucket ^CountAndSum count-and-sum]
  IDeref
  (deref [this]
    (let [buckets       (map #(.sum %)  bucket-adders)
          bucket-labels (map #(hash-map :le (str %)) bucket-sizes)
          all-but-inf         (zipmap bucket-labels buckets)]
            (assoc all-but-inf {:le "+Inf"} (.sum total-bucket))))
  Observable
  (observation! [this value]
    (doseq [[size bucket] (map list bucket-sizes bucket-adders)] (when (<= value size) (.add bucket 1)))
    (.add total-bucket 1)
    (count-and-sum! count-and-sum value))
  Updates
  (updates [_this]
    (.updates count-and-sum))
  Sum
  (sum [_this]
    (.sum count-and-sum)))

(defmethod observe!
  Histogram
  ([this value & {:keys [with-labels] :or {with-labels {}}}]
    (if (empty? with-labels)
      (observation! this value)
      (throw (Exception. "No labels are possible for simple histogram!")))))

(defn- create-histogram! [buckets ^CountAndSum count-and-sum]
  (map->Histogram {:bucket-sizes  (sort buckets)
                   :bucket-adders (for [_ (range (count buckets))] (DoubleAdder.))
                   :total-bucket  (DoubleAdder.)
                   :count-and-sum count-and-sum}))

(defn histogram-collector-fn [id description labels buckets]
  #(let [c-a-s (CountAndSum. (DoubleAdder.) (DoubleAdder.))]
     (map->Collector {:id                       id
                      :description              description
                      :type                     :histogram
                      :label-values->collectors (ConcurrentHashMap.)
                      :labels                   (set labels)
                      :metric-fn                (partial create-histogram! buckets c-a-s)
                      :count-and-sum            c-a-s})))

(defn histogram [id &
                 {description :description
                  buckets     :buckets
                  labels      :with-labels
                  registry    :registry
                  :or
                              {buckets     [0.005 0.01 0.025 0.05 0.075 0.1 0.25 0.5 0.75 1 2.5 5 7.5 10]
                               labels      []
                               description ""
                               registry    default-registry}}]
  (when (contains? (set labels) "le")
    (throw (IllegalArgumentException. "'le' is a reserved label for buckets.")))
  (let [collector (register-or-return! registry id :histogram
                                       (histogram-collector-fn id description labels buckets))]
    (if (empty? labels)
      (get-or-create-metric! collector {})
      collector)))

(defmethod print-method Gauge [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmethod print-method Counter [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmethod print-method Histogram [h ^Writer writer]
  ((get-method print-method IRecord) h writer))