(ns kernel-time.search
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [ajax.core :refer [GET]]
            [cljs.core.async :refer [<! >! chan put!]]
            [cljs.nodejs :as n]
            [clojure.string :refer [join]]))

;; yuck
(extend-type array
  ISeqable
  (-seq [this] (array-seq this 0)))

(def K (n/require "node-kickass"))

(defn kickass->show [j]
  {:title (-> j (aget "title"))
   :seeds (-> j (aget "torrent:seeds") (aget "#"))
   :peers (-> j (aget "torrent:peers") (aget "#"))
   :hash (-> j (aget "torrent:infohash") (aget "#"))
   :magnet (-> j (aget "torrent:magneturi") (aget "#"))})

(defn kickass [params]
  (let [out (chan)]
    (-> (K.)
        (.setQuery (:query params))
        (.setPage (:page params))
        (.run (fn [errors data]
                (put! out (mapv kickass->show data)))))
    out))

(defn yts->movie [j]
  {:title (j "MovieTitleClean")
   :magnet (j "TorrentMagnetUrl")
   :hash (j "TorrentHash")
   :image (j "CoverImage")
   :rating (j "MovieRating")
   :size (j "Size")
   :imdb (j "ImdbCode")})

(defn yts [params]
  (let [ch (chan)]
    (GET "http://yts.re/api/list.json"
         {:params {:keywords (:query params)
                   :limit (:limit params)
                   :sort (:sort params)
                   :set (+ 1 (:page params))}
          :handler (fn [resp] (put! ch (map yts->movie (resp "MovieList"))))})
    ch))

(def trakt-key "4e3c6770d8c44fa6775720ba3014dc84")

(defn trakt-movie-summaries [imdbs]
  (let [out (chan)
        url (str "http://api.trakt.tv/movie/summaries.json"
                 "/"
                 trakt-key
                 "/"
                 (join "," imdbs))]
    (GET url {:handler (partial put! out)})
    out))

(defn trakt-trending-shows []
  (let [out (chan)
        url (str "http://api.trakt.tv/shows/trending.json/" trakt-key)]
    (GET url {:handler (partial put! out)})
    out))

(defn search [params]
  (let [out (chan)
        y (yts params)
        k (kickass params)]
    (go (let [movies (<! y)
              shows (<! k)]
          ;;summaries (<! (trakt-summaries (map :imdb movies)))
          (>! out (vec (concat movies shows)))))
    out))
