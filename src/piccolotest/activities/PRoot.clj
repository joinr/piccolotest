;;Extension of piccolo's PRoot class
;;to optionally use event-driven
;;time.
(ns piccolotest.activities.PRoot
  (:require [clojure.core.async :as async]
            [piccolotest.activities [Timer :as tmr]])
  (:import  [org.piccolo2d PRoot]
            [java.awt.event ActionListener]
            [piccolotest.activities Timer]
            ))

;;Can we shift to more of a stream-based
;;view of time?  The only time we're
;;doing this is during non-interactive
;;rendering, i.e. when we're spitting out
;;movies (typically).
;;The flow of control goes:
;;activity requests timer at delay,
;;root creates timer, which starts pumping
;;action messages at interval rate.
;;action messages cause input to accumulate,
;;thus causing evaluation of processinputs
;;at points in the future.

;;Ignoring where timers come from...
;;We have a stream of action events, causing
;;input, at various points of time.
;;So, tuples of [t, action]....
;;where t is the global time, as contiguous
;;chunks of time change.  Can we simply inject
;;an event queue here? And step through the
;;queue as necessary?

;;Frames are computed as time changes.
;;So, beginning of day, we have a bunch of
;;action events (due to one or more timers
;;firing).  We probably need to know what
;;time(s) are pending...
;;So, another way to do this is to provide
;;time-elapsed, and convert deltas into
;;sub-frames.

;;Since activities decide internally if they should
;;step, during processActivity, they maintain their
;;own reference on time.  I "Think" this will work...
;;we just go on updating time like normal.
(gen-class
   :extends org.piccolo2d.PRoot
   :implements [clojure.lang.IMeta
                clojure.lang.IObj
                clojure.lang.IDeref                
                ]
   :name     piccolotest.activities.PRoot
   :state   state
   :init    init
   :constructors {[Object] []
                  [] []}
   :exposes-methods {})

;;I think all we have to do here is override
;;the proot's getGlobalTime, and
;;createTimer.

;;this is somewhat hackish at the moment...
(defn -init
  ([time-source]
     [[] ;;calls to pass to super.
      {
       :source  time-source
       :metadata (atom {})
       }]))

(defn -deref    [^piccolotest.activities.PRoot this]
  (.state this))

(defn -withMeta [^piccolotest.activities.PRoot this m]
  (do (when-let [oldm  (:metadata (.state this))]
        (reset!  oldm m))
      this))

(defn -meta     [^piccolotest.activities.PRoot this]
  (when-let [m (:metadata (.state this))]
    @m))

(defn -getGlobalTime [^piccolotest.activities.PRoot  pr]
  (deref (:source (.deref pr))))


(defn ^Timer -createTimer [^piccolotest.activities.PRoot  pr
                           ^long delay
                           ^ActionListener listener]
  (tmr/->timer (:source (.deref pr))))


(defn ->proot [src]
   (piccolotest.activities.PRoot. src))
