(ns piccolotest.core
  (:import [piccolotest.GraphEditor]
           [javax.swing JFrame]))

(defn -main
  "Roughly corresponds to the Java code in GraphEditorTester.java"
  []
  (let [window (JFrame.)
        ge (piccolotest.GraphEditor. 500 500)]
    (println "... got to beginning of let")
    (doto window
      (.setTitle "Piccolo Graphics Editor")
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))
    (.add (.getContentPane window) ge)
    (println "... got to adding Graphics Editor to window")
    (doto window
      (.pack)
      (.setVisible true)))
  (println "Goodbye!"))


