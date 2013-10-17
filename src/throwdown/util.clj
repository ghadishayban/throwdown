(ns throwdown.util
  (:require [clojure.core.reducers :as r]))

;; Reimplementation of Java's (.split Pattern) to be zero-allocation
;; NB: Only difference from Java's impl is that this will return
;;     possible "" strings at the end. Make sure to r/filter those.
;; Also this doesn't respect "reduced"
(defn split-string [^CharSequence s ^java.util.regex.Pattern re]
  (reify clojure.core.protocols/CollReduce
    (coll-reduce [_ f init]
      (let [m (re-matcher re s)
            [acc i] (loop [acc init i 0]
                          (if (.find m)
                            (let [match (.toString (.subSequence s i (.start m)))]
                              (recur (f acc match)
                                     (.end m)))
                            [acc i]))]
        (if (zero? i)
          init
          (f acc (.toString (.subSequence s i (.length s)))))))))

;; 2x normal merge-with
;; this one is not n-arity
(defn transient-merge-with
  [f ma mb]
  (persistent! (reduce-kv
                (fn [a k v]
                  (if (contains? ma k)  ;; NB against original collection
                    (assoc! a k (f (get ma k) v))
                    (assoc! a k v)))
                (transient ma)
                mb)))
