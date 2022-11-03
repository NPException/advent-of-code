(ns lab.throttle
  (:import (java.util.concurrent TimeUnit)))


(defn ^:private leaky-update!
  "update function returns true iff the throttle currently has spare usages"
  [^long bucket-size ^long dec-time last-update$ used$]
  ;; locking ensures that only a single thread at a time can modify the throttle
  (locking used$
    (let [decrements (-> (System/nanoTime) (- ^long @last-update$) (quot dec-time))
          ^long used (if (<= decrements 0)
                       @used$
                       (do
                         ;; forward last update to most recent decrement timestamp
                         (vswap! last-update$ (fn [^long prev]
                                                (+ prev (* decrements dec-time))))
                         ;; update `used` counter based on passed time
                         (vswap! used$ (fn [^long used]
                                         (if (> decrements used)
                                           0
                                           (- used decrements))))))]
      (boolean
        (when (< used bucket-size)
          (vreset! used$ (inc used)))))))


(defn leaky-throttle
  "Decrements the used up amount continuously over time.
  The entire amount can be exhausted in a burst,
  and then continue in a throttled manner. (leaky bucket)"
  [^long bucket-size calls-per-unit ^TimeUnit unit]
  (let [dec-time     (max 1 (long (/ (.toNanos unit 1) calls-per-unit)))
        last-update$ (volatile! (System/nanoTime))
        used$        (volatile! 0)]
    (fn [f]
      (when (leaky-update! bucket-size dec-time last-update$ used$)
        (f)))))
