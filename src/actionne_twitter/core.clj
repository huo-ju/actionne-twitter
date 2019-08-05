(ns actionne_twitter.core
    (:require
            [clojure.tools.logging :as log]
            [config.core :refer [env]]
            [twttr.api :as api]
            [twttr.auth :refer :all]
            [cheshire.core :refer :all]
            [clj-http.client :as client]
            [clojure.java.io :as io]
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

(let [wtr (agent (BufferedWriter. (FileWriter. (str homedir "/data/" "actionne_twitter-" today ".backup.log") true)))]
    (defn save [msg]
      (letfn [(write [out msg]
              (.write out msg) 
              (.flush out)
                    out)]
          (send wtr write msg)))
      (defn close []
            (send wtr #(.close %))))


(defn loadsession [account]
    (let [filename (str homedir "/data/" account "-actionne_twitter-session.clj")]
        (try
            (read-string (slurp filename))
            (catch Exception e {}))
    )
)

(defn updatesession [account session]
    (log/info (str "updatesession: " homedir "/data/" account "-actionne_twitter-session.clj"))
    (log/info (str "session: " session))
    (let [filename (str homedir "/data/" account "-actionne_twitter-session.clj")]
        (with-open [w (clojure.java.io/writer filename)]
            (binding [*out* w]
        (pr session)))
    )
)

(defn initsession [account]
    (updatesession account {:stage "init"})
)

(defn paramsname [stage]
    (if (= "init" stage)
        "min"
        "max"
    )
)

(defn timelineparams [session]
    (if (= "init" (:stage session))
        (if (not= (:confirmed session) nil)  ;init stage, with id
            {:max_id (:last_id (:confirmed session))}
            {} 
        )
        (if (not= (:confirmed session) nil)  ;normal stage, with id
            {:since_id (:last_id (:confirmed session))}
            {} 
        )
    ))


(defn mediaurls [tweet]
    (let [media (:media (:extended_entities tweet))]
        (if (not (nil? media))
            (vec (map :media_url_https media))
            []
        )
    )
)

(defn httprequest [url]
  (let [req (client/get url {:as :byte-array :throw-exceptions false})]
    (if (= (:status req) 200)
      (:body req)))
)

(defn download [url filename]
    (let [body (httprequest url)]
        (if (not= nil body)
            (if (= nil (io/copy body (java.io.File. filename)))
                true
                false
            )
            false
        )
    )
)

(defn backup [original]
    (log/info (str "backup: " (:id original)))
    (let [urls (mediaurls original)]
        (let [result (mapv (fn [url] 
            (download url (str homedir "/data/" (:id original) "_" (#(nth % (dec (count %))) (clojure.string/split url #"/"))))
        ) urls)]
            (if (= nil (some #(= false %) result))
                (do (save (generate-string original))
                    (save "\n"))
                (log/error (str "backup error: " (:id original)))
            )
        )
    )
)
(defn deletetweet [id env]
    (try
        (let [creds (map->UserCredentials env) screen_name (:screen_name env)]
            (log/info (str "delete tweet: " id ))
            (api/statuses-destroy-id creds :params {:id id})
            nil
        )
    (catch Exception e 
        (log/error (str "delete tweet: " id " " (:cause (Throwable->map e))))
        nil
    ))
)

(defn delete [config id original]
    (log/info (str "delete: " (:id original)))
    (if (or (= false (:dryloop (:actionne_twitter config))) (nil? (:dryloop (:actionne_twitter config))))
        (if (= true (:backup (:actionne_twitter config)))
            (doall (backup original)
                (deletetweet id (:actionne_twitter config))
            )
        )
        (log/info (str "dryloop action delete, skip " id))
    )
)


(defn notify [config id original]
    (log/info (str "notifiy: " id))
    (if (or (= false (:dryloop (:actionne_twitter config))) (nil? (:dryloop (:actionne_twitter config))))
        (if (= true (:backup (:actionne_twitter config)))
            (backup original)
        )
        (log/info (str "dryloop action notify, skip " id))
    )
)

(defn lastid[session]
    (if (not= (:confirmed session) nil) 
        (:confirmed session)
        {}
    )
)




(defn fetchtweets [env session]
    (let [creds (map->UserCredentials env) screen_name (:screen_name env)]
        (log/info (str "fetch: " (merge {:screen_name screen_name} (timelineparams (loadsession screen_name)))))
        (let [formattedtweets (let [tweets (api/statuses-user-timeline creds :params (merge {:screen_name screen_name} (timelineparams (loadsession screen_name))))]
                (map (fn [tweet] 
                        (let [  favorite_count (:favorite_count tweet) 
                                id (:id_str tweet) 
                                retweet_count (:retweet_count tweet)  
                                text (:text tweet)
                                created_at (.getTime  (.parse df (:created_at tweet)))
                                media_urls (mediaurls tweet)
                                retweeted_status (:retweeted_status tweet)
                                in_reply_to_status_id (:in_reply_to_status_id tweet)
                                in_reply_to_user_id (:in_reply_to_user_id tweet)
                             ]

                           {:id id 
                            :object {:id id :favorite_count favorite_count :retweet_count retweet_count :text text :created_at created_at 
                                     :media_urls media_urls
                                     :category (cond 
                                                    (or (not (nil? in_reply_to_status_id))  (not (nil? in_reply_to_user_id))) "reply" 
                                                    (not (nil? retweeted_status)) "retweet" 
                                                    :else "tweet")
                                    } 
                            :original tweet})) 
                tweets)
            )]
            (log/info (str "tweets count:" (count formattedtweets)))
            (if (> (count formattedtweets) 0)
                (let [maxid (apply (resolve (symbol (paramsname (:stage (loadsession screen_name))))) (map read-string (map :id formattedtweets)))]
                    (updatesession screen_name (assoc session :pending {:last_id maxid})))
                (if (= "init" (:stage (loadsession screen_name)))
                    (doall (updatesession screen_name (assoc session :stage "normal"))
                    (log/info (str "Init stage finished. Turn to the normal stage."))))
            )
            formattedtweets
        )
    )
)
(defn before [env]
    (log/info (str "call actionne_twitter/core.before for account: " (:screen_name env)))
    (if (= {} (loadsession (:screen_name env)))
        (initsession (:screen_name env))    
    )
)


(defn success [env]
    (log/info (str "call actionne_twitter/core.success for account: " (:screen_name env)))
    (let [session (loadsession (:screen_name env))]
        (if (not= (:pending session) nil) 
           (updatesession (:screen_name env) (assoc session :confirmed (:pending session)))
        )
    )
)

(defn failure [env]
    (log/info (str "call actionne_twitter/core.failure for account: " (:screen_name env)))
)


(defn run [env]
    (log/info (str "call actionne_twitter/core.run for account: " (:screen_name env)))
    (let [session (loadsession (:screen_name env))]
       (fetchtweets env session)
    )
)

