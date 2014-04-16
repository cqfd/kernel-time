(ns kernel-time.search
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [ajax.core :refer [GET]]
            [cljs.core.async :refer [<! >! chan put!]]
            [clojure.string :refer [join]]))

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
         {:params params
          :handler (fn [resp] (put! ch (map yts->movie (resp "MovieList"))))})
    ch))

(def trakt-key "4e3c6770d8c44fa6775720ba3014dc84")

(defn trakt-summaries [imdbs]
  (let [out (chan)]
    (let [url (str "http://api.trakt.tv/movie/summaries.json"
                   "/"
                   trakt-key
                   "/"
                   (join "," imdbs))]
       (GET url {:handler (partial put! out)}))
    out))

(defn search [params]
  (let [out (chan)]
    (go (let [movies (<! (yts params))
              summaries (<! (trakt-summaries (map :imdb movies)))]
          (>! out (mapv (fn [m s]
                          (assoc m :summary s))
                        movies summaries))))
    out))
