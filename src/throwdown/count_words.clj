(ns throwdown.count-words
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

;; benchmark with better jvm opts DONE

(set! *warn-on-reflection* true)

(def regex #"\s+")

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

(defn freq-by-fold [coll]
  (r/fold
   4096
   (r/monoid (fn [a b] (merge-with + a b))
             hash-map)
   (fn [acc word]
     (assoc acc word (inc (get acc word 0))))
   coll))

;; 4000ms
(defn impl1
  [body]
  (freq-by-fold  (str/split body regex)))

;; 1800ms
(defn impl2
  [body]
  (freq-by-fold (split-string body regex)))

;; 1700ms
(defn impl3
  [body]
  (frequencies    (str/split body regex)))

;; 1500ms
(defn impl4
  [body]
  (frequencies    (split-string body regex)))



(defn transient-merge [ma mb]
  (persistent! (reduce-kv
                (fn [a k v] (assoc! a k v))
                (transient ma)
                mb)))

;; 500ms vs 1000ms for normal
(defn transient-merge-with [f ma mb]
  (persistent! (reduce-kv
                (fn [a k v]
                  (if (contains? ma k)  ;; NB against original collection
                    (assoc! a k (f (get ma k) v))
                    (assoc! a k v)))
                (transient ma)
                mb)))


(defn freq-by-fold-trans [coll]
  (r/fold
   128000
   (r/monoid (fn [a b] (transient-merge-with + a b))
             hash-map)
   (fn [acc word]
     (assoc acc word (inc (get acc word 0))))
   coll))

;; 2100ms lots of variance
(defn impl5
  [body]
  (freq-by-fold-trans (str/split body regex)))

;; 2050ms
(defn impl6
  [body]
  (freq-by-fold-trans (into [] (str/split body regex))))
