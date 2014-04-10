(ns kernel-time.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! timeout]]))

(enable-console-print!)

(def request (js/require "request"))
;;(def torrent-stream (js/require "torrent-stream"))
(def peerflix (js/require "peerflix"))

(def app-state (atom {:src nil}))

;; get-request :: URL -> Chan Resp
(defn get-request [url]
  (let [ch (chan)]
    (request url (fn [error response body]
                   (println "We got something!!")
                   (put! ch [error response body])))
    ch))

(def magnet "magnet:?xt=urn:btih:8C5043DF1A8FEC9F7FDDDA70617C6BB96E14E6CC&dn=game+of+thrones+s04e01+hdtv+x264+killers+ettv&tr=http%3A%2F%2Ftracker.ex.ua%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337")

(let [engine (peerflix magnet #js {:buffer (str (* 1.5 1024 1024))
                                   :connections 100})]
  (. js/console dir engine)
  (. js/console dir (.-on engine))
  (.on engine "ready" (fn []
                        (println "Engine is ready...")
                        (.listen (.-server engine) 8888)))
  (.on engine "listening"
       (fn []
         (println "We're listening....")
         (let [server (.-server engine)
               href (str "http://127.0.0.1:"
                         (.-port (.address server))
                         "/")]
           (update-in app-state [:src]
                      (fn [_] href))
           (println @app-state)))))


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
