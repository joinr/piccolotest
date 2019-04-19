;;Extension of basic javax.swing.Timer
;;to use an event-driven view of
;;time, triggering action events
;;in response to changes in
;;a global clock.
(ns piccolotest.activities.Timer
  (:require [clojure.core.async :as async])
  (:import [javax.swing.Timer]
           [java.awt.event ActionListener ActionEvent]
           ))
    
(gen-class
   :extends javax.swing.Timer
   :implements [clojure.lang.IMeta
                clojure.lang.IObj
                clojure.lang.IDeref                
                ]
   :name    piccolotest.activities.Timer
   :state   state
   :init    init
   :constructors {[Object] [int java.awt.event.ActionListener]}
   :exposes-methods {start          parentStart
                     stop           parentStop
                     setRepeats     parentRepeats
                     restart        parentRestart
                     isRunning      parentRunning
                     fireActionPerformed parentFire})

;;So...
;;For our timer, we'd like to observe a stream of events
;;and define time in that way.  A channel is a logical
;;device, or a reference.
;;Anything we can watch...

;;either we map a channel to times...
;;or we map changes in an atom to times....
;;Alternatively, we have the timer provide a
;;callback to trigger notifications...
;;An event-based timer has no set delay....
(defn begin-watching [nm source f cancel?]
  (cond  (instance? clojure.lang.Atom source)
         (add-watch source nm (fn [r k old new]
                                (if-not (cancel?)
                                  (f new)
                                  (remove-watch r k))))
         (instance? clojure.core.async.impl.channels.ManyToManyChannel source)
         (async/go-loop []
           (when-let [res (and (not (cancel?)) (async/<! source) )] ;then we should stop.
             (do (f res)
                 (recur))))
         :else (throw (Exception. (str [:unknown-class source])))))     
;;this is somewhat hackish at the moment...
(defn -init [time-source]
  (let [;;notification...
        cmd "TIME_CHANGED"
        running (atom true)
        ;;we fire action events, which tell subscribers to
        ;;look at the time.
       nm     (gensym "timer")
        ]
    [[(int 0)
      nil
      ] ;;calls to pass to super.
     {:name    nm
      :source  time-source
      :running running
      :metadata (atom {})
      }]))


(defn -deref    [^piccolotest.activities.Timer this]
  (.state this))

(defn -withMeta [^piccolotest.activities.Timer this m]
  (do (when-let [oldm  (:metadata (.state this))]
        (reset!  oldm m))
      this))
(defn -meta     [^piccolotest.activities.Timer this]
  (when-let [m (:metadata (.state this))]
    @m))

;;not restartable...
(defn -start [^piccolotest.activities.Timer this] this)
(defn -stop [^piccolotest.activities.Timer this] (reset! (.state this) false) this) 
(defn -restart [^piccolotest.activities.Timer this] this)
(defn -setRepeats [^piccolotest.activities.Timer this ^Boolean v] this)

(defn setup [^piccolotest.activities.Timer o]
  (let [;;notification...
        cmd "TIME_CHANGED"
        running (:running (.state o))
        notify (fn notify [t]
                 (.parentFire o (ActionEvent. o ActionEvent/ACTION_PERFORMED cmd (long t) 0)))
        ;;we fire action events, which tell subscribers to
        ;;look at the time.
        nm     (:name (.state o))
        time-source (:source (.state o))
        ;;if it's an atom, we add a watch.
        _     (begin-watching nm time-source notify (fn [] (not @running)))
        ;;if it's a channel, we add a go...
        ]
    o))

(defn ->timer [src]
  (setup (piccolotest.activities.Timer. src)))
