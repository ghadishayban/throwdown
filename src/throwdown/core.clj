(ns throwdown.core
  (:require

   ;; your fav http library here
   [org.httpkit.client :as httpkit]

   ;; a few core.async primitives
   [clojure.core.async :refer (<!! <! >! put! go chan thread)]

   ;; IO manip
   [clojure.java.io :as io]

   [clojure.core.reducers :as r]
   [clojure.string :as str]

   ;; misc helpers
   [throwdown.util :refer (split-string transient-merge-with)]))

(set! *warn-on-reflection* true)

(defn GET
  "Hit a server and put the result body in a channel"
  [uri channel]
  (httpkit/get uri
               {:keepalive 50000}
               #(put! channel (get % :body))))  ;; NB node.js: the only callback

(defn frequency-of-words
  [body]
  (frequencies (split-string body #"\s+")))

(defn beat-node [urls]
  (let [n (count urls)
        http-bodies (chan n)
        freq-maps (chan n)]

    ;; grab http bodies, stick in a chan
    ;; for each payload, compute the frequency of the words
    (doseq [url urls]
      (GET url http-bodies)
      (go (>! freq-maps (frequency-of-words (<! http-bodies)))))

    ;; collect each map of frequency and merge together
    (<!! (thread
          (loop [result {} i n]
            (if (pos? i)
              (recur (transient-merge-with + result (<!! freq-maps))
                     (dec i))
              result))))))

;; ...more hubris
(def beat-erlang beat-node)
(def beat-golang beat-node)

(defn write-output
  [freq-map]
  (with-open [f (io/writer "output.txt" :buffer-size 200000)]
    (doseq [[word freq] freq-map]
      (.write f (str word ": " freq "\n")))))

(defn -main
  [file-name]
  (with-open [f (io/reader file-name)]
    (let [urls (->> (line-seq f)
                    (r/map str/trim)
                    (into []))]
      (-> urls
          beat-node
          write-output))))
