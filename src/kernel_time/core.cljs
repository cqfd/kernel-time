(ns kernel-time.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [kernel-time.search :refer [search]]
            [cljs.core.async :refer [<! chan put! sliding-buffer timeout]]
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

(def app-state (atom {:movies [] :idx 0}))

(defn tick [ms]
  (let [out (chan (sliding-buffer 1))]
    (go (while true 
          (<! (timeout ms))
          (>! out :tick)))
    out))

(defn throttle [t c]
  (let [out (chan)]
    (go (loop [throttled true]
          (if throttled
            (do (<! t) (recur false))
            (do (>! out (<! c)) (recur true)))))
    out))

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
      (if (> (count (:movies data)) 0)
        (let [movie ((:movies data) (:idx data))
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
                                    (dom/img #js {:src (:image movie)})
                                    (dom/h2 nil (:title movie))))
                          (:movies data))))))

(om/root
  (fn [app owner]
    (reify
     om/IInitState
     (init-state [_]
       {:text ""
        :query (chan (sliding-buffer 1))})
      om/IWillMount
      (will-mount [_]
        ;; start the server
        (go (let [server (.createServer http)
                  magnet->f (atom {})]
              (.on server "request"
                   (fn [req res]
                     (let [params (.-query (.parse url (.-url req) true))
                           magnet (.-magnet params)
                           f (@magnet->f magnet)]
                       (if f
                         ((handle-f f) req res)
                         (go (let [f (<! (torrent->f magnet))]
                               (swap! magnet->f assoc magnet f)
                               ((handle-f f) req res)))))))
              (.listen server 8082)))
        ;; watch the search box
        (let [state (om/get-state owner)
              query (throttle (tick 2000) (:query state))]
          (go (while true
                (let [movies (<! (search {:limit 20 :sort "seeds" :keywords (<! query)}))]
                  (println "Movies!" movies)
                  (om/update! app :movies movies))))))
      om/IRenderState
      (render-state [this state]
        (dom/div nil
         (dom/input #js {:type "text"
                         :value (:text state)
                         :onChange (fn [e]
                                     (om/set-state! owner :text (.. e -target -value))
                                     (put! (:query state) (.. e -target -value)))})
          (om/build video-widget app)
          (om/build movie-list app)))))
  app-state
  {:target (. js/document (getElementById "app"))})
