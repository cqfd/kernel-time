(ns kernel-time.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [ajax.core :refer [GET]]
            [cljs.core.async :refer [put! chan <! timeout]]
            [cljs.nodejs :as n]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def t (n/require "torrent-stream"))
(def http (n/require "http"))
(def pump (n/require "pump"))
(def qs (n/require "querystring"))
(def range-parser (n/require "range-parser"))
(def url (n/require "url"))

(def app-state (atom {:list [] :idx 0}))

(defn yts [params]
  (let [ch (chan)]
    (GET "http://yts.re/api/list.json" {:params params :handler (partial put! ch)})
    ch))

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

(defn handle-f [f]
  (fn [req res]
    (let [range (aget (range-parser (.-length f) (.. req -headers -range)) 0)
          content-length (+ 1 (- (.-end range) (.-start range)))
          content-range (str "bytes " (.-start range) "-" (.-end range) "/" (.-length f))]
      (set! (.-statusCode res) 206)
      (.setHeader res "Accept-Ranges" "bytes")
      (.setHeader res "Content-Length" content-length)
      (.setHeader res "Content-Range" content-range)
      (pump (.createReadStream f range) res))))

(defn video-widget [data owner]
  (reify
    om/IRender
    (render [this]
      (if (> (count (:list data)) 0)
        (let [movie ((:list data) (:idx data))
              magnet (:magnet movie)
              src (str "http://127.0.0.1:8082/?" (.stringify qs #js {:magnet magnet}))]
          (dom/video #js {"src" src "controls" true}))
        (dom/div nil)))))

(defn movie-list [data owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/ul nil
             (map-indexed (fn [i movie]
                            (dom/li #js {:className (when (= i (:idx data)) "selected")
                                         :onClick (fn [e] (om/update! data :idx i))}
                                    (:title movie)))
                          (:list data))))))

(om/root
  (fn [app owner]
    (reify
      om/IWillMount
      (will-mount [_]
        ; start the server
        (go (let [server (.createServer http)
                  magnet->f (atom {})]
              (.on server "request"
                   (fn [req res]
                     ; url.parse(req.url).query
                     ; (.. url (parse (.-url req)) -query)
                     (let [params (.-query (.parse url (.-url req) true))
                           magnet (.-magnet params)
                           f (@magnet->f magnet)]
                       (if f
                         ((handle-f f) req res)
                         (go (let [f (<! (torrent->f magnet))]
                               (swap! magnet->f assoc magnet f)
                               ((handle-f f) req res)))))))
              (.listen server 8082)))
        ; fetch some torrents
        (go (let [resp (<! (yts {:limit 2 :sort "seeds"}))
                  movies (resp "MovieList")]
              (om/update! app :list (vec (map (fn [m] {:title (m "MovieTitleClean")
                                                       :magnet (m "TorrentMagnetUrl")}) movies))))))
      om/IRender
      (render [_]
        (dom/div nil
          (om/build video-widget app)
          (om/build movie-list app)))))
  app-state
  {:target (. js/document (getElementById "app"))})
