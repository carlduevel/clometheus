(ns clometheus.core
  (:import (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic DoubleAdder)
           (clojure.lang Keyword IDeref IRecord PersistentHashSet)
           (java.io Writer)
           (clometheus CKMSQuantiles$Quantile TimeWindowQuantiles)))

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

(def ^:private valid-metric-name-re #"[a-zA-Z_:][a-zA-Z0-9_:]*")
(defn- validate-metric-name [name]
  (when-not (re-matches valid-metric-name-re name)
    (throw (IllegalArgumentException. (str "Invalid metric name: '" name
                                           "'. Metric name has to match this regex: [a-zA-Z_:][a-zA-Z0-9_:]*")))))
(let [valid-label-re    #"[a-zA-Z_][a-zA-Z0-9_]*"
      reserved-label-re #"__.*"]
  (defn- validate-label-name [label-name]
    (when-not (re-matches valid-label-re label-name)
      (throw (IllegalArgumentException.
               (str "Invalid label name: '" label-name
                    "'.\n Label name has to match this regex: [a-zA-Z_][a-zA-Z0-9_]*"))))
    (when (re-matches reserved-label-re label-name)
      (throw (IllegalArgumentException.
               (str "Invalid label name: '" label-name
                    "'.\n Label names beginning with two underscores are reserved for internal use."))))))




(defn- validate-label-names [label-names]
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

(alter-meta! #'map->CountAndSum assoc :private true)
(alter-meta! #'->CountAndSum assoc :private true)

(defrecord Collector [^String id ^String description ^Keyword type ^ConcurrentHashMap label-values->collectors
                      ^PersistentHashSet labels metric-fn ^CountAndSum count-and-sum]
  ICollector
  (id [_this] id)
  (description [_this] description)
  (metric-type [_this] type)
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
  (get-or-create-metric! [_this key]
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

(alter-meta! #'map->Collector assoc :private true)
(alter-meta! #'->Collector assoc :private true)


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

(alter-meta! #'map->Counter assoc :private true)
(alter-meta! #'->Counter assoc :private true)

(defn- counter-collector-fn [id description labels]
  (fn [] (map->Collector {:id                       id
                          :description              description
                          :type                     :counter
                          :label-values->collectors (ConcurrentHashMap.)
                          :labels                   (set labels)
                          :metric-fn                #(Counter. (DoubleAdder.))})))
(defn counter
  ([id & {description :description labels :labels registry :registry
          :or         {labels [] description "" registry default-registry}}]
   (let [collector (register-or-return! registry id :counter (counter-collector-fn id description labels))]
     (if (empty? labels)
       (get-or-create-metric! collector {})
       collector))))

(defprotocol Decrementable
  (decrement! [this] [this value]))

(defprotocol Resettable
  (reset! [this value]))

(defrecord CallbackGauge [callback-fn]
  IDeref
  (deref [_this] (callback-fn)))

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

(alter-meta! #'map->Gauge assoc :private true)
(alter-meta! #'->Gauge assoc :private true)


(defn- gauge-collector-fn [id description labels callback-fn]
  (fn [] (map->Collector
           {:id                       id
            :description              description
            :type                     :gauge
            :label-values->collectors (ConcurrentHashMap.)
            :labels                   (set labels)
            :metric-fn                (if callback-fn #(CallbackGauge. callback-fn) #(Gauge. (DoubleAdder.)))})))

(defn gauge
  [id & {description :description labels :labels registry :registry callback-fn :callback-fn
         :or         {labels [] description "" registry default-registry}}]
  (when (and callback-fn (seq labels))
    (throw (IllegalArgumentException. "Callback gauges must not have labels.")))
  (let [collector (register-or-return! registry id :gauge (gauge-collector-fn id description labels callback-fn))]
    (if (empty? labels)
      (get-or-create-metric! collector {})
      collector)))

(defprotocol Observable
  (observation! [this value]))

(defn- type-dispatch-one-or-more-args
  ([this]
   (type this))
  ([this & _opts]
   (type this)))

(defn- type-dispatch-2-or-more-args
  ([this _val]
   (type this))
  ([this _val & _opts]
   (type this)))

(defmulti inc! type-dispatch-one-or-more-args)
(defmulti dec! type-dispatch-one-or-more-args)
(defmulti observe! type-dispatch-2-or-more-args)
(defmulti set! type-dispatch-2-or-more-args)

(defmethod inc!
  Counter
  ([this & {:keys [labels by] :or {labels {} by 1}}]
    (if (empty? labels)
      (increment! this by)
      (throw (Exception. "No labels are possible for simple counter!")))))

(defmethod inc!
  Gauge
  ([this & {:keys [labels by] :or {labels {} by 1}}]
    (if (empty? labels)
      (increment! this by)
      (throw (Exception. "No labels are possible for simple gauge!")))))

(defmethod dec!
  Gauge
  ([this & {:keys [labels by] :or {labels {} by 1}}]
    (if (empty? labels)
      (decrement! this by)
      (throw (Exception. "No labels are possible for simple gauge!")))))

(defmethod set!
  Gauge
  ([this val & {:keys [labels] :or {labels {}}}]
    (if (empty? labels)
      (reset! this val)
      (throw (Exception. "No labels are possible for simple gauge!")))))

(defn- validate-labels [^Collector collector labels->values]
  (when-not (= (.labels collector) (set (keys labels->values)))
    (throw (IllegalArgumentException.
             (str "Wrong or insufficient labels provided. All and only these must be set: " (.labels collector))))))

(defmethod inc!
  Collector
  ([this & {:keys [labels by] :or {labels {} by 1}}]
    (validate-labels this labels)
    (increment! (get-or-create-metric! this labels) (or by 1))))

(defmethod dec!
  Collector
  ([this & {:keys [labels by] :or {labels {} by 1}}]
    (validate-labels this labels)
    (decrement! (get-or-create-metric! this labels) (or by 1))))

(defmethod observe!
  Collector
  ([this val & {:keys [labels] :or {labels {}}}]
    (validate-labels this labels)
    (observation! (get-or-create-metric! this labels) val)))

(defmethod set!
  Collector
  ([this val & {:keys [labels] :or {labels {}}}]
    (validate-labels this labels)
    (reset! (get-or-create-metric! this labels) val)))



(defrecord Histogram [bucket-sizes bucket-adders ^DoubleAdder total-bucket ^CountAndSum count-and-sum]
  IDeref
  (deref [_this]
    (let [buckets       (map #(.sum %) bucket-adders)
          bucket-labels (map #(hash-map :le (str %)) bucket-sizes)
          all-but-inf   (zipmap bucket-labels buckets)]
      (assoc all-but-inf {:le "+Inf"} (.sum total-bucket))))
  Observable
  (observation! [_this value]
    (doseq [[size bucket] (map list bucket-sizes bucket-adders)] (when (<= value size) (.add bucket 1)))
    (.add total-bucket 1)
    (count-and-sum! count-and-sum value))
  Updates
  (updates [_this]
    (.updates count-and-sum))
  Sum
  (sum [_this]
    (.sum count-and-sum)))

(alter-meta! #'map->Histogram assoc :private true)
(alter-meta! #'->Histogram assoc :private true)


(defmethod observe!
  Histogram
  ([this value & {:keys [labels] :or {labels {}}}]
    (if (empty? labels)
      (observation! this value)
      (throw (Exception. "No labels are possible for simple histogram!")))))

(defn- create-histogram! [buckets ^CountAndSum count-and-sum]
  (map->Histogram {:bucket-sizes  (sort buckets)
                   :bucket-adders (for [_ (range (count buckets))] (DoubleAdder.))
                   :total-bucket  (DoubleAdder.)
                   :count-and-sum count-and-sum}))

(defn- histogram-collector-fn [id description labels buckets]
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
                  labels      :labels
                  registry    :registry
                  :or
                              {buckets     [0.005 0.01 0.025 0.05 0.075 0.1 0.25 0.5 0.75 1 2.5 5 7.5 10]
                               labels      []
                               description ""
                               registry    default-registry}}]
  (when (contains? (set labels) "le")
    (throw (IllegalArgumentException. "'le' is a reserved label for histograms.")))
  (let [collector (register-or-return! registry id :histogram
                                       (histogram-collector-fn id description labels buckets))]
    (if (empty? labels)
      (get-or-create-metric! collector {})
      collector)))

(defrecord Summary [^CountAndSum count-and-sum ^TimeWindowQuantiles time-window-quantiles]
  IDeref
  (deref [_this]
    (let [all-quantiles (map #(.quantile %) (.quantiles time-window-quantiles))
          bucket-labels (map #(hash-map :quantile (str %)) all-quantiles)
          quantile-vals (map #(.get time-window-quantiles %) all-quantiles)]
      (zipmap bucket-labels quantile-vals)))
  Observable
  (observation! [_this value]
    (.insert time-window-quantiles value)
    (count-and-sum! count-and-sum value))
  Updates
  (updates [_this]
    (.updates count-and-sum))
  Sum
  (sum [_this]
    (.sum count-and-sum)))

(alter-meta! #'map->Summary assoc :private true)
(alter-meta! #'->Summary assoc :private true)

(defn- prob? [^double prob]
  (<= 0.0 prob 1.0))

(defn- validate-quantile [^double quantile, ^double error]
  (when-not (prob? quantile)
    (throw (IllegalArgumentException. "Quantile must be a value between 0.0 and 1.0.")))
  (when-not (prob? error)
    (throw (IllegalArgumentException. "Error must be a value between 0.0 and 1.0."))))


(defn quantile [^double quantile, ^double error]
  (validate-quantile quantile error)
  (CKMSQuantiles$Quantile. quantile error))

(defn- summary-collector-fn [id description labels ^TimeWindowQuantiles twq]
  #(let [c-a-s (CountAndSum. (DoubleAdder.) (DoubleAdder.))]
     (map->Collector {:id                       id
                      :description              description
                      :type                     :summary
                      :label-values->collectors (ConcurrentHashMap.)
                      :labels                   (set labels)
                      :metric-fn                (fn [] (Summary. c-a-s twq))
                      :count-and-sum            c-a-s})))

(defn summary [id &
               {description           :description
                labels                :labels
                registry              :registry
                ^long max-age-seconds :max-age-seconds
                ^int age-buckets      :age-buckets
                quantiles             :quantiles
                :or
                                      {max-age-seconds 600
                                       age-buckets     5
                                       quantiles       []
                                       description     ""
                                       registry        default-registry}}]
  (when (contains? (set labels) "quantile")
    (throw (IllegalArgumentException. "'quantile' is a reserved label for summaries.")))
  (when (<= max-age-seconds 0)
    (throw (IllegalArgumentException. "Max-age-seconds must be positive.")))
  (when (<= age-buckets 0)
    (throw (IllegalArgumentException. "Age-buckets must be positive.")))
  (let [quantiles (into-array CKMSQuantiles$Quantile quantiles)
        twqs      (TimeWindowQuantiles. quantiles max-age-seconds age-buckets)
        collector (register-or-return! registry id :summary
                                       (summary-collector-fn id description labels twqs))]
    (if (empty? labels)
      (get-or-create-metric! collector {})
      collector)))

(defmethod observe!
  Summary
  ([this value & {:keys [labels] :or {labels {}}}]
    (if (empty? labels)
      (observation! this value)
      (throw (Exception. "No labels are possible for simple Summary!")))))

(defmethod print-method Gauge [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmethod print-method Counter [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmethod print-method Histogram [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmethod print-method CallbackGauge [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmethod print-method Summary [h ^Writer writer]
  ((get-method print-method IRecord) h writer))

(defmacro timed [observer  & body]
  `(let [start# (System/currentTimeMillis)]
    (try
      ~@body
      (finally
        (observe! ~observer (* (- (System/currentTimeMillis) start#) 0.001))))))
