(ns kernel-time.server
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan put! sliding-buffer timeout]]))

(def t (n/require "torrent-stream"))
(def http (n/require "http"))
(def pump (n/require "pump"))
(def range-parser (n/require "range-parser"))
(def url (n/require "url"))

(defn torrent->f [torrent]
  (let [ch (chan)
        engine (t torrent)]
    (.on engine "ready"
         (fn []
           (let [biggest (.. engine -files
                             (reduce (fn [a b]
                                       (if (> (.-length a) (.-length b))
                                         a
                                         b))))]
             (put! ch biggest))))
    ch))

(defn handle [f req res]
  (let [range (aget (range-parser (.-length f) (.. req -headers -range)) 0)
        content-length (+ 1 (- (.-end range) (.-start range)))
        content-range (str "bytes " (.-start range) "-" (.-end range) "/" (.-length f))]
    (set! (.-statusCode res) 206)
    (.setHeader res "Accept-Ranges" "bytes")
    (.setHeader res "Content-Length" content-length)
    (.setHeader res "Content-Range" content-range)
    ;; pipe
    (pump (.createReadStream f range) res)))

(defn start []
  (go (let [server (.createServer http)
            magnet->f (atom {})]
        (.on server "request"
             (fn [req res]
               (let [params (.-query (.parse url (.-url req) true))
                     magnet (.-magnet params)
                     f (@magnet->f magnet)]
                 (if f
                   (handle f req res)
                   (go (let [f (<! (torrent->f magnet))]
                         (swap! magnet->f assoc magnet f)
                         (handle f req res)))))))
        (.listen server 8080))))
