(ns kernel-time.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println js/videojs)

(def app-state (atom {:text "Hello y'all!"}))

(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/video nil "Hi there!"))))

(om/root
  (fn [app owner]
    (dom/div nil
      (om/build widget app)))
  app-state
  {:target (. js/document (getElementById "app"))})
