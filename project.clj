(defproject piccolotest "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [spork "0.1.9.5-SNAPSHOT"]
                 [org.piccolo2d/piccolo2d-core "3.0"]
                 [org.piccolo2d/piccolo2d-extras "3.0"]]
    :jvm-opts ^:replace ["-server"])
