(ns kernel-time.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ajax.core :refer [GET]]
            [cljs.core.async :refer [put! chan <! timeout]]))

(enable-console-print!)

(def t (n/require "torrent-stream"))
(def http (n/require "http"))
(def pump (n/require "pump"))
(def range-parser (n/require "range-parser"))

(def app-state (atom {:src nil
                      :list []
                      :idx 0}))

(defn yts [params]
  (let [ch (chan)]
    (GET "http://yts.re/api/list.json"
         {:params params
          :handler (partial put! ch)})
    ch))

(def magnet "magnet:?xt=urn:btih:C922A6BD4DC2FD9B50CF2BC91F35B9DB8009600E&dn=house+of+cards+2013+s02e01+webrip+hdtv+x264+2hd+rartv&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337")

(defn torrent->f [torrent]
  (let [ch (chan) engine (t torrent)]
    (.on engine "ready" (fn []
                          (println (.-length (.-files engine)))
                          (put! ch (aget (.sort (.-files engine)
                                                (fn [a b] (- (.-length b) (.-length a))))
                                         0))))
    ch))

(defn handle-f [f]
  (fn [req res]
    (let [range (aget (range-parser (.-length f) (.-range (.-headers req))) 0)
          content-length (+ 1 (- (.-end range) (.-start range)))
          content-range (str "bytes " (.-start range) "-" (.-end range) "/" (.-length f))]
      (println "Handling a request!")
      (set! (.-statusCode res) 206)
      (.setHeader res "Accept-Ranges" "bytes")
      (.setHeader res "Content-Length" content-length)
      (.setHeader res "Content-Range" content-range)
      (pump (.createReadStream f range) res))))

;; Maybe this would be better for the component to maintain some
;; internal state for the video source? Real data = magnet link?

(def counter (atom 0))

;; {magnet f}
(defn video-widget [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (go (let [server (.createServer http)
                _gross (<! (timeout 6000))]
            (.on server "request"
                 (fn [req res]
                   (go (let [movie ((:list @app) (:idx @app))
                             f (<! (torrent->f (:magnet movie)))]
                         ((handle-f f) req res)))))
            (.listen server 9193)
            (om/transact! app :src (fn [_] (str "http://127.0.0.1:9193"))))))
    om/IRender
    (render [this]
      (swap! counter inc)
      (println "Rerendering the video widget..." @counter)
      (dom/div nil
        (dom/video #js {"src" (:src app) "controls" true})))))

(defn movie-list [app owner]
  (reify
    om/IWillMount
    (will-mount [this]
      (go (let [resp (<! (yts {:limit 2 :sort "seeds"}))
                movies (resp "MovieList")]
            (om/update! app :list (vec (map (fn [m] {:title (m "MovieTitleClean")
                                                     :magnet (m "TorrentMagnetUrl")}) movies))))))
    om/IRender
    (render [this]
      (println app)
      (apply dom/ul nil
             (map-indexed (fn [i movie]
                            (dom/li #js {:className (when (= i (:idx app)) "selected")
                                         :onClick (fn [e]
                                                    (println "Handling click!")
                                                    (om/update! app :idx i))}
                                    (:title movie)))
                          (:list app))))))

(om/root
  video-widget
  app-state
  {:target (. js/document (getElementById "app"))})

(om/root
  movie-list
  app-state
  {:target (. js/document (getElementById "list"))})
