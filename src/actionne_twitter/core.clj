(ns actionne-twitter.core
    (:require
            [config.core :refer [env]]
            [twttr.api :as api]
            [twttr.auth :refer :all]
            [cheshire.core :refer :all]
    )
    (:import (java.io BufferedWriter FileWriter))
)

(def homedir
    (or (env :sg-home) "/tmp/sghome")
)  

(def today
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (new java.util.Date))
)

(let [wtr (agent (BufferedWriter. (FileWriter. (str homedir "/backup/" "actionne-twitter-" today ".log") true)))]
    (defn save [msg]
      (letfn [(write [out msg]
              (.write out msg) 
              (.flush out)
                    out)]
          (send wtr write msg)))
      (defn close []
            (send wtr #(.close %))))


(defn backup [original]
    (save (generate-string original))
    (save "\n")
)

(defn delete [id original]
    (println (str "twitter delete: " id))

)

(defn done []
    (close)
)


(defn notify [id original]
    (println (str "twitter notify: " id))
    (println (str "twitter backup: " id))
    (backup original)
)


(defn fetchtweets [env]
    (let [creds (map->UserCredentials env )]
        (let [tweets (api/statuses-user-timeline creds :params {:screen_name "virushuo"})]
            (map (fn [tweet] 
                    (let [  favorite_count (:favorite_count tweet) 
                            id (:id_str tweet) 
                            retweet_count (:retweet_count tweet)  
                            text (:text tweet)  
                            in_reply_to_status_id (:in_reply_to_status_id tweet)
                            in_reply_to_user_id (:in_reply_to_user_id tweet)

                         ]

                       {:id id 
                        :object {:favorite_count favorite_count :retweet_count retweet_count :text text
                                 :category (if (or (not (nil? in_reply_to_status_id))  (not (nil? in_reply_to_user_id)))
                                     "tweet"
                                     "reply")} 
                        :original tweet})) 
            tweets)
        )
    )
)

