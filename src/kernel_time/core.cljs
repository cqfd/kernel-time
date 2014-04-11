(ns kernel-time.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! timeout]]))

(enable-console-print!)

(def t (n/require "torrent-stream"))
(def http (n/require "http"))
(def pump (n/require "pump"))

(def app-state (atom {:src nil}))

(def magnet "magnet:?xt=urn:btih:8C5043DF1A8FEC9F7FDDDA70617C6BB96E14E6CC&dn=game+of+thrones+s04e01+hdtv+x264+killers+ettv&tr=http%3A%2F%2Ftracker.ex.ua%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337")

(defn torrent->stream [torrent]
  (let [ch (chan)
        engine (t torrent)]
    (.on engine "ready"
         (fn []
           (let [f (aget (.-files engine) 1)]
             (. js/console dir f)
             (put! ch (.createReadStream f)))))
    ch))

(defn serve [torrent]
  (go (let [stream (<! (torrent->stream torrent))
            server (.createServer http
                                  (fn [_req res]
                                    (pump stream res
                                          (fn [err]
                                            (.dir js/console err)))))]
        (.on server "close" (fn [& rest]
                              (println "server closed!")))
        (.listen server 8880)
        (println "Listening...")
        (swap! app-state update-in [:src] (fn [_] "http://127.0.0.1:8880/"))
        )))

(serve magnet)

(go (while true
      (<! (timeout 2000))
      (println "Tick...")))

(defn video-widget [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/video #js {"src" (:src data) "controls" true}))))

(om/root
  (fn [app owner]
    (dom/div nil
      (om/build video-widget app)))
  app-state
  {:target (. js/document (getElementById "app"))})
