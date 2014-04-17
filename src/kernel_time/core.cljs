(ns kernel-time.core
  (:require-macros [cljs.core.async.macros :refer [alt! go]])
  (:require [kernel-time.search :refer [kickass search]]
            [kernel-time.server :as server]
            [cljs.core.async :refer [<! chan put! sliding-buffer timeout]]
            [cljs.nodejs :as n]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(server/start)

(def qs (n/require "querystring"))

(def app-state (atom {:movies [] :idx nil}))

(defn seconds [n] (* 1000 n))

(defn after-at-least [ms c]
  (let [out (chan)]
    (go (loop [v (<! c) t (timeout ms)]
          (alt!
            t (do (>! out v)
                 (recur (<! c) (timeout ms)))
            c ([v] (recur v (timeout ms))))))
    out))

(defn video-widget [data owner]
  (reify
    om/IRender
    (render [this]
      (if (:idx data)
        (let [movie ((:movies data) (:idx data))
              magnet (:magnet movie)
              src (str "http://127.0.0.1:8080/?" (.stringify qs #js {:magnet magnet}))]
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
        (let [state (om/get-state owner)
              query (after-at-least (seconds 2) (:query state))]
          (go (while true
                (let [movies (<! (search {:limit 20 :sort "seeds" :query (<! query)}))]
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
