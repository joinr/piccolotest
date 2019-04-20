;;provides a javafx webengine as a simple
;;node for use in piccolo.
(ns piccolotest.browser
  (:require ;[fx-clj.core :as fx]
            [spork.cljgui.components [swing :as swing]])
  (:import [javafx.application Platform]
           [javafx.beans.value ChangeListener
            ObservableValue]
           [javafx.embed.swing JFXPanel]
           [javafx.event EventHandler]
           [javafx.scene Scene]
           [javafx.scene.web WebEngine WebEvent WebView]
           [java.net MalformedURLException URL]
           [javax.swing JFrame JPanel Box BoxLayout JTextField JSplitPane
            JLabel JButton JOptionPane JScrollPane Timer SwingUtilities
            JFileChooser JTable JFrame JMenuBar JMenu JMenuItem JTextArea
            JList ListModel ListSelectionModel AbstractListModel JTabbedPane JComponent ImageIcon
            JProgressBar BorderFactory
            SwingUtilities]
           ;[javax.swing.event ChangeListener]
           [java.awt GridBagConstraints GridBagLayout BorderLayout FlowLayout 
                     GridLayout Component Graphics Graphics2D Dimension Insets]           
           [java.awt.event ActionListener MouseListener ComponentListener
            MouseAdapter MouseEvent WindowAdapter WindowEvent HierarchyListener HierarchyEvent ActionEvent]))

;;duplicated...
(defn invoke-later! [^java.lang.Runnable f]
  (SwingUtilities/invokeLater f))
;;duplicated...
(defmacro do-scene
  "Ensures that any alterations of a live scene are handled on 
   the swing EDT thread, where appropriate.  For other backends this may
   be unncessary."
  [& body]
  `(invoke-later! (~'fn [] (do ~@body))))

(defmacro do-fx
  "Ensures that any alterations of a live scene are handled on 
   the swing EDT thread, where appropriate.  For other backends this may
   be unncessary."
  [& body];
  `(let [x# (atom nil)
         _#  (Platform/runLater  (~'fn [] (let [res# ~@body]
                                            (reset! x# res# ))))]
     (deref x#)))


(defn create-scene [{:keys [lblStatus
                            txtURL
                            progressBar
                            panel
                            jfxPanel
                            ] :as ssb}]
  (let [view   (WebView.)
        engine (.getEngine view)]
     (-> engine
         (.titleProperty)
         (.addListener
          (reify ChangeListener
            (changed [this  observable  oldValue newValue]
                                        ;                       (do-scene
                                        ;                         (.setTitle  newValue
              ))))
     (-> engine
         (.setOnStatusChanged 
                   (reify EventHandler
                     #_(^void handle [this ^WebEvent event]
                        (do-scene (.setText lblStatus (.getData event)))
                        )
                     (handle [this event]
                       (do-scene (.setText lblStatus (.getData event)))
                       ))))
     (-> engine
         (.locationProperty)
         (.addListener
          (reify ChangeListener
            (changed [this observable oldValue newValue]
              (do-scene
               (.setText txtURL newValue))
              )
            #_(^void changed [this ^ObservableValue observable ^String oldValue ^String newValue]
               (do-scene
                (.setText txtURL newValue))
               ))))
     (-> engine
         (.getLoadWorker)                  
         (.workDoneProperty)
         (.addListener
          (reify ChangeListener
            (changed [this  observable oldValue  newValue]
              (do-scene
               (.setValue progressBar (.intValue newValue)))
              )
            #_(^void changed [this ^ObservableValue observable ^Number oldValue ^Number newValue]
               (do-scene
                (.setValue progressBar (.intValue newValue)))
               ))))
     (-> engine
         (.getLoadWorker)                  
         (.exceptionProperty)
         (.addListener
          (reify ChangeListener
            (changed [this observable  old  value]
              (do-scene
               (JOptionPane/showMessageDialog
                panel
                (str (.getLocation engine) "\n"
                             (if value (.getMessage value)
                                 "Unexpected error."))
                "Loading error..."
                JOptionPane/ERROR_MESSAGE)
               )
              #_(^void changed [this ^ObservableValue observable ^Throwable old ^Throwable value]
                      (do-scene
                       (JOptionPane/showMessageDialog
                        panel
                        (str (.getLocation engine) "\n"
                             (if value (.getMessage value)
                                 "Unexpected error."))
                        "Loading error..."
                        JOptionPane/ERRORMESSAGE)
                       ))))))
     (.setScene jfxPanel (Scene. view))
     (assoc ssb
            :engine engine
            :view view)))

(defn toURL [^String s]
  (try (.toExternalForm (URL. s) )
   (catch MalformedURLException e nil)))
     
(defn loadURL [^WebEngine engine ^String url]
  (do-fx
   (let [tmp (toURL url)
         tmp (or tmp (str (toURL "http://" url)))]
     (.load engine tmp))))

(defn ->ssb []
  (let [^JFXPanel jfxPanel (JFXPanel.)
        ^WebEngine engine (WebEngine.)        
        lblStatus (JLabel.)
        btnGo     (JButton. "Go")
        txtURL    (JTextField.)
        progressBar  (doto (JProgressBar.)
                       (.setPreferredSize (Dimension. 150 18))
                       (.setStringPainted true))
        al  (reify java.awt.event.ActionListener
              (^void actionPerformed [this ^ActionEvent e]
                (loadURL (.getText txtURL))))
        _   (.addActionListener btnGo al)
        _   (.addActionListener txtURL  al)
        topBar (doto (JPanel. (BorderLayout. 5 0))
                 (.setBorder (BorderFactory/createEmptyBorder 3 5 3 5))
                 (.add txtURL BorderLayout/CENTER)
                 (.add btnGo  BorderLayout/EAST))
        statusbar (doto (JPanel. (BorderLayout. 5 0))
                    (.setBorder (BorderFactory/createEmptyBorder 3 5 3 5))
                    (.add lblStatus BorderLayout/CENTER)
                    (.add progressBar BorderLayout/EAST))
        panel    (doto (JPanel. (BorderLayout.))
                   (.add topBar BorderLayout/NORTH)
                   (.add jfxPanel BorderLayout/CENTER)
                   (.add statusbar BorderLayout/SOUTH))]
    (create-scene  {:lblStatus lblStatus
                    :txtURL txtURL
                    :progressBar progressBar
                    :panel panel 
                    :jfxPanel jfxPanel
                    })))

(comment 
(defn browser [ & {:keys [init-url]}]
  (let [wv     (fx/web-view)
        we     (.getEngine wv)
        address-bar (fx/text-field :#input)
        url    (fr/atom (or init-url ""))
        tprop       (.textProperty address-bar)
        _      (fx/bind-> tprop url)
        load!  (fn [& args] (.load we @url))
        _      (when init-url
                 (do
                   (.setText address-bar init-url)
                   (.load we init-url)))
        load-btn (fx/button :#reload-btn {:text "Load Page"
                                          :on-action load!})]
    {:webview wv
     :address-bar address-bar
     :url url
     :load-btn load-btn
     :browser-node
     (fx/v-box
      (fx/h-box address-bar load-btn)
      wv)}))
)
#_(defn browser [ & {:keys [init-url]}]
  (let [wv     (fx/web-view)
        we     (.getEngine wv)
        address-bar (fx/text-field :#input)
        url    (fr/atom (or init-url ""))
        tprop       (.textProperty address-bar)
        _      (fx/bind-> tprop url)
        load!  (fn [& args] (.load we @url))
        _      (when init-url
                 (do
                   (.setText address-bar init-url)
                   (.load we init-url)))
        load-btn (fx/button :#reload-btn {:text "Load Page"
                                          :on-action load!})]
    {:webview wv
     :address-bar address-bar
     :url url
     :load-btn load-btn
     :browser-node
     (fx/v-box
      (fx/h-box address-bar load-btn)
      wv)}))
