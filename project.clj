(defproject actionne_twitter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]  [twttr "3.2.2"] [cheshire "5.8.1"]  [yogthos/config "1.1.1"] [clj-http "3.10.0"] [java-http-clj "0.4.0"] [clj-soup/clojure-soup "0.1.3"]]
  :profiles {:uberjar {:aot :all :uberjar-name "actionne_twitter.jar"} }
  :repl-options {:init-ns actionne_twitter.core})
