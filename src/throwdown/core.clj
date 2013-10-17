(ns throwdown.core
  (:require [org.httpkit.client :as httpkit]
            [clojure.core.async :refer (<!! <! >! put! go chan)]
            [throwdown.count-words :as count-words])
  (:import [java.util.concurrent CountDownLatch]))

(def urls
  (mapv (partial str "http://en.wikipedia.org/wiki/")
        ["ISO_8601"
         "Bonobos"
         "Clojure"
         "Rich_Hickey"
         "JavaScript"
         "Sumner_White"
         "Palafrugell"
         "De_minimis_fringe_benefit"
         "Apple_Remote_Desktop"]))


(defn http-kit [uri channel]
  (httpkit/get uri
            {:keepalive 50000}
            #(put! channel (get % :body))))

(defn watch-req [request-fn uri]
  (let [bt (System/currentTimeMillis)
        rc (chan 1)]
    (request-fn uri rc)
    (println "time in ms: " (- (System/currentTimeMillis) bt))
    (println "Size body" (count (<!! rc)))))

(defn freqall [urls]
  (let [n (count urls)
        return-ch (chan n)
        result (atom {})
        latch (CountDownLatch. n)]

    ;; fire off async requests
    (doseq [url urls]
      (http-kit url return-ch))

    ;; collect results
    (dotimes [_ n]
      (let [body (<!! return-ch)]
        (future
          (let [freqs (count-words/impl4 body)]
            (swap! result #(count-words/transient-merge-with + % freqs))
            (.countDown latch)))))
    (.await latch)
    @result))

(let [chunk (<!! rc)]
  (future (let [c (compute)
                #(swap! res  merge-with + c)])))


(defn -main
  [file-name]
  (with-open [f (io/reader file-name)]))
