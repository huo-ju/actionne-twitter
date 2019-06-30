(ns actionne_twitter.core
    (:require
            [config.core :refer [env]]
            [twttr.api :as api]
            [twttr.auth :refer :all]
            [cheshire.core :refer :all]
    )
    (:import (java.io BufferedWriter FileWriter) (java.util Date Locale) )
)

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(def homedir 
  (expand-home (or (env :actionne-home) "~/actionne")))

(def today
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (new java.util.Date))
)

(def df 
    (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss ZZZZZ yyyy" (Locale. "english"))
)

(let [wtr (agent (BufferedWriter. (FileWriter. (str homedir "/data/" "actionne_twitter-" today ".log") true)))]
    (defn save [msg]
      (letfn [(write [out msg]
              (.write out msg) 
              (.flush out)
                    out)]
          (send wtr write msg)))
      (defn close []
            (send wtr #(.close %))))

(defn loadsession []
    (let [filename (str homedir "/data/" "actionne_twitter-session.clj")]
        (try
            (read-string (slurp filename))
            (catch Exception e {}))
    )
)

(defn updatesession [session]
    (let [filename (str homedir "/data/" "actionne_twitter-session.clj")]
        (with-open [w (clojure.java.io/writer filename)]
            (binding [*out* w]
        (pr session)))
    )
)

(defn initsession []
    (updatesession {:stage "init"})
)

(defn paramsname [stage]
    (if (= "init" stage)
        "min"
        "max"
    )
)

(defn timelineparams [session]

    (if (= "init" (:stage session))
        (if (not= (:pending session) nil)  ;init stage, with id
            {:max_id (:last_id (:confirmed session))}
            {} 
        )
        (if (not= (:pending session) nil)  ;normal stage, with id
            {:since_id (:last_id (:confirmed session))}
            {} 
        )
    ))


(defn backup [original]
    (println (str "save.." (:id original)))
    ;(if (contains? (:entities original) :media)
    ;    (do (println "===media")
    ;    (prn (:media (:entities original))))
    ;)
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


(defn lastid[session]
    (if (not= (:confirmed session) nil) 
        (:confirmed session)
        {}
    )
)

(defn mediaurls [tweet]
    (let [media (:media (:extended_entities tweet))]
        (if (= nil media)
            []
            (vec (map :media_url media))
        )
    )
)
(defn fetchtweets [env session]
    ;(prn env)
    (let [creds (map->UserCredentials env) 
          screen_name (:screen_name env)]
        ;(if (= (:confirmed session) nil) 
        ;    ;{:screen_name (:screen_name env)} 
        ;    ;{:max_id (:last_id session) :screen_name (:screen_name env)}
        ;    (:confirmed (loadsession))
        ;)
        (prn (lastid session))
        (prn (timelineparams (loadsession)))

        (let [formattedtweets (let [tweets (api/statuses-user-timeline creds :params (merge {:screen_name screen_name} (timelineparams (loadsession))))]



                (map (fn [tweet] 
                        (let [  favorite_count (:favorite_count tweet) 
                                id (:id_str tweet) 
                                retweet_count (:retweet_count tweet)  
                                text (:text tweet)
                                created_at (.getTime  (.parse df (:created_at tweet)))
                                media_urls (mediaurls tweet)
                                in_reply_to_status_id (:in_reply_to_status_id tweet)
                                in_reply_to_user_id (:in_reply_to_user_id tweet)
                             ]

                           {:id id 
                            :object {:favorite_count favorite_count :retweet_count retweet_count :text text :created_at created_at 
                                     :media_urls media_urls
                                     :category (if (or (not (nil? in_reply_to_status_id))  (not (nil? in_reply_to_user_id)))
                                        "reply"
                                        "tweet")} 
                            :original tweet})) 
                tweets)
            )]
            (if (> 0 (count formattedtweets))
                (let [maxid (apply (resolve (symbol (paramsname (:stage (loadsession))))) (map read-string (map :id formattedtweets)))]
                    (updatesession 
                        (assoc session :pending {:last_id maxid})
                    )
                )
            )
            formattedtweets
        )
    )
)


(defn confirmtask[]
    (let [session (loadsession)]
        (if (not= (:pending session) nil) 
           (updatesession (assoc session :confirmed (:pending session)))
        )
    )
)


(defn run [env]
    (println "run")
    (let [session (loadsession)]
       (fetchtweets env session)
       ;(if (= (:last_id (loadsession)) nil) (println "nil") (println "id"))
    )

)

