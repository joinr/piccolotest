(defproject piccolotest "0.1.3-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [spork "0.2.1.1-SNAPSHOT"]
                 [org.piccolo2d/piccolo2d-core "3.0"]
                 [org.piccolo2d/piccolo2d-extras "3.0"]
                 [org.clojure/core.async "0.4.490"]
                 [joinr/swingrepl "1.4.2-SNAPSHOT"]]
  :source-paths ["src" "../spork/src"]
  :jvm-opts ^:replace ["-server"]
  )
