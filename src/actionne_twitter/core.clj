(ns actionne_twitter.core
    (:require
            [clojure.tools.logging :as log]
            [config.core :refer [env]]
            [hickory.core :as hickory]
            [hickory.select :as s]
            [twttr.api :as api]
            [twttr.auth :refer :all]
            [cheshire.core :refer :all]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [java-http-clj.core  :as http ]
    )
    (:import (java.io BufferedWriter FileWriter) (java.util Date Locale) (java.net CookieManager ProxySelector URI URLEncoder) (org.jsoup Jsoup) (org.jsoup.select Elements) (org.jsoup.nodes Element) )
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

(defn initsession [account watching]
    (updatesession account {:stage "init" :method "api" :watching watching})
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
            {:max_id (- (:last_id (:confirmed session)) 1)}
            {} 
        )
        (if (not= (:confirmed session) nil)  ;normal stage, with id 
            {:since_id (+ (:last_id (:confirmed session)) 1)}
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
  (let [resp (client/get url {:as :byte-array :throw-exceptions false})]
    (if (= (:status resp) 200)
      (:body resp)))
)


(def h {"User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36" }) 

(defn extratext [inputcontent]
    (clojure.string/join " " (map (fn [t] 
        (if (= (type t) java.lang.String)
            t
            (if (= (type (:content t)) clojure.lang.PersistentVector) (apply str (:content t)) (extratext (first (:content t))))
        )
        ) inputcontent))
)

(defn parsenode [htmlnode]
        (let [id (-> (s/select (s/child (s/tag :li)) htmlnode) first :attrs :data-item-id)
            text (-> (s/select (s/and (s/class "tweet-text") (s/tag :p)) htmlnode) first :content)
            created_at (-> (s/select (s/and (s/class "_timestamp") (s/tag :span)) htmlnode) first :attrs :data-time)
            retweet_count (-> (s/select (s/child (s/class "ProfileTweet-action--retweet") (s/class "ProfileTweet-actionCount")) htmlnode) first :attrs :data-tweet-stat-count)
            favorite_count (-> (s/select (s/child (s/class "ProfileTweet-action--favorite") (s/class "ProfileTweet-actionCount")) htmlnode) first :attrs :data-tweet-stat-count)
            content (-> (s/select (s/child (s/tag :li)) htmlnode) first :content)
            tweetattrs (-> (s/select (s/child (s/and (s/attr :data-component-context) (s/tag :div))) htmlnode) first :attrs)

            imgs (-> (s/select (s/child (s/and (s/class "AdaptiveMedia-photoContainer") (s/tag :div)) (s/tag :img)) htmlnode) )
            
             ]

            {
                    :id id
                    :object {
                        :id id :favorite_count (Integer/parseInt favorite_count) :retweet_count (Integer/parseInt retweet_count) :created_at (Integer/parseInt created_at)
                        :text (extratext text)
                        :category (cond 
                            (= "true" (:data-is-reply-to tweetattrs)) "reply" 
                            (not (nil? (:data-retweet-id tweetattrs))) "retweet" 
                        :else "tweet")
                        :media_urls (into [] (map (fn [img]  (:src (:attrs img)) ) imgs))
                    }
                    :original {
                        :id_str (str id) :favorite_count favorite_count :retweet_count retweet_count :created_at created_at 
                        :text (extratext text) 
                        :extended_entities { :media (into [] (map (fn [img] {:media_url_https (:src (:attrs img)) :type "photo"}) imgs))}
                    }
                }
        )
)

(defn fetchtweetsweb [env session]
    (let [cookie-handler (CookieManager.) screen_name (:screen_name env) search_term (:search_term env)]
        (def client (http/build-client {:follow-redirects :normal :cookie-handler cookie-handler :headers {"User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36" }}))

        (if (= "web" (:method session))
            (let [uri (format "http://twitter.com/i/search/timeline?f=tweets&q=%s%%20include:nativeretweets%s" (URLEncoder/encode 
                (if (not (nil? (:last_tweet_timestamp (:confirmed session))))
                    (str search_term " until:" (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (* 1000 (:last_tweet_timestamp (:confirmed session))))) 
                    search_term
                ) "UTF-8")
            (if (not (nil? (:last_id (:confirmed session))))
                (str "&max_position=" (:last_id (:confirmed session)))
                "&max_position="
            ))]
                (println (str "url:" uri))
                (let [resp (http/send {:uri uri :method :get} {:client client})] 
                    (if (= (:status resp) 200)
                        (do 
                            (some
                                (fn [v]  
                                    (if (= (key v) "items_html")
                                        (let [tweets (map (fn [input] 
                                            (let [htmlnodes (hickory.core/as-hickory input) ]
                                                (if (= (type htmlnodes) clojure.lang.PersistentArrayMap)
                                                    (parsenode htmlnodes)
                                                )
                                        )) (hickory.core/parse-fragment (val v)))]
                                            (let [formattedtweets (remove nil? tweets)]
                                                (log/info (str "tweets count:" (count formattedtweets)))
                                                (let [
                                                        last_tweet_date (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (* 1000 (:created_at (:object (last formattedtweets)))))
                                                        first_tweet_timestamp (/ (inst-ms (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd") (:first_tweet env))) 1000)
                                                        last_tweet_timestamp (:created_at (:object (last formattedtweets)))
                                                      ]

                                                    (if  (= (count formattedtweets) 0) 
                                                        (if (not (< first_tweet_timestamp last_tweet_timestamp))
                                                            (if (= "init" (:stage (loadsession screen_name)))
                                                                (do (updatesession screen_name {:watching (:watching session) :stage "normal" :method "api"}) (log/info (str "web fetch tweets finished. Turn to the normal api mode.")))
                                                                (updatesession screen_name {:watching (:watching session) :stage "normal" :method "api"})
                                                            )
                                                            (let [current_session (loadsession screen_name)]
                                                                (updatesession screen_name (assoc current_session :pending (assoc (:pending current_session) :last_tweet_timestamp  (- last_tweet_timestamp 86400)))))
                                                        
                                                        )))
                                                formattedtweets 
                                            ))
                                        (do 
                                            (if (= (key v) "min_position")
                                               (do  
                                                (let [current_session (loadsession screen_name)]
                                                    (updatesession screen_name (assoc current_session :pending (assoc (:pending current_session) :last_id (val v)))))))))) (parse-string (:body resp))))))))))

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
    (let [session (loadsession (:screen_name env))]

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




(defn fetchtweetsapi [env session]
    (let [creds (map->UserCredentials env) screen_name (:screen_name env) method (:method env)]
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
                    (updatesession screen_name (assoc session :pending {:last_id maxid :last_timestamp (:created_at (:object (last formattedtweets)))}))) ;TODO return max_id with timestamp
                (if (= "init" (:stage (loadsession screen_name)))
                    (do (updatesession screen_name {:watching (:watching session) :stage "init" :method "web"}) (log/info (str "api fetch tweets finished. Turn to the web mode.")))
                    (updatesession screen_name {:watching (:watching session) :stage "normal" :method "api"})
                )
            )
            formattedtweets
        )
    )
)

(def time-to-seconds
    { 
      "minute" `60
      "minutes" `60
      "hour" `3600
      "hours" `3600
      "day" `86440
      "days" `86440
})

(defn before [env]
    (log/info (str "call actionne_twitter/core.before for account: " (:screen_name env)))
    (let [watching (:watching env)]
        (let [timevec (clojure.string/split watching #" ") ]
            (if (= {} (loadsession (:screen_name env)))
                (initsession (:screen_name env) (time-to-seconds (second timevec)))
            )
        )
    )
)


(defn success [env]
    (log/info (str "call actionne_twitter/core.success for account: " (:screen_name env)))
    (let [session (loadsession (:screen_name env))]
        (if (not= (:pending session) nil) 
           (doall 
                (let [watching (if (nil? (:watching session)) 86440 (:watching session))]  ;default watching range is 5 days
                    (if (and (= (:stage session) "normal") (> (- (- (/ (.getTime (new java.util.Date)) 1000) (:last_timestamp (:pending session))) (:watching session)) 0))
                    (updatesession (:screen_name env) {:watching (:watching session) :stage (:stage session)})
                    (updatesession (:screen_name env) (assoc session :confirmed (:pending session)))
                )
            )
           )
        )
    )
)

(defn failure [env]
    (log/info (str "call actionne_twitter/core.failure for account: " (:screen_name env)))
)


;(defn run [env]
;    (log/info (str "call actionne_twitter/core.run for account: " (:screen_name env)))
;    (let [session (loadsession (:screen_name env))]
;       (fetchtweets env session)
;    )
;)


(defn run [env]
    (log/info (str "call actionne_twitter/core.run for account: " (:screen_name env)))
    (let [session (loadsession (:screen_name env))]
        (cond 
            (= "api" (:method session))
                (fetchtweetsapi env session)
            (= "web" (:method session))
                (fetchtweetsweb env session)
        )
    )
)

