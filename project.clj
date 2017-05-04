(defproject piccolotest "0.1.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;:aot [piccolotest.GraphEditor]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [spork "0.2.0.2-SNAPSHOT"]
                 [org.piccolo2d/piccolo2d-core "3.0"]
                 [org.piccolo2d/piccolo2d-extras "3.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [joinr/swingrepl "1.4.2-SNAPSHOT"]]
    :jvm-opts ^:replace ["-server"])
