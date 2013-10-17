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

(def beat-erlang beat-node)
(def beat-golang beat-node)

(def sample-urls
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

(defn write-output
  [freq-map]
  (reduce-kv (fn [_ word freq]
               (println word ": " freq))
             nil
             freq-map))

(defn -main
  [file-name]
  (with-open [f (io/reader file-name)]
    (let [urls (->> (line-seq f)
                    (r/map str/trim)
                    (into []))]
      (-> urls
          beat-node
          write-output))))
