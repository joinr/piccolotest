;;Wrappers for the general PActivity classes in
;;piccolo2d.  One departure from the stock
;;piccolo2d stuff is the desire to have
;;activities respond to external events.
(ns piccolotest.activities
  (:require [piccolotest.activities [Timer :as tmr]])
  (:import [org.piccolo2d.activities PActivity PInterpolatingActivity]
           [piccolotest.activities Timer]))
        

;;note...
;;The paradigm of time and activities in Piccolo2d goes through
;;the swing framework pretty extensively.
;;Conceptually,
;;PRoot manages time, specifically a global-time relative to
;;system/current-time-millis.
;;So everytime we process inputs (from swing events), we update
;;the global time.
;;From there, we have a list of activities that are scheduled.
;;Some are active (i.e. their interval of existence intersects
;;with the current time).
;;These listeners are asked for their next time (based on
;;an internal implementation of time steps), and are
;;scheduled to fire when the specific time hits.
;;To ensure that specific times hit, javax.swing.timer(s)
;;are created to schedule actionevent(s) that are pushed
;;as input onto the proot's event queue.  So, activities
;;cause the side effect of timers to be created so as
;;to ensure their timesteps are traversed.

;;All of this correlates to global time, which is driven
;;by system/current-time-millis and the swing event
;;handler for piccolo.

;;So the global time drives the processing of activities,
;;if we hack that (say by extending proot and overriding
;;its computation of global time to be derived from
;;some source "other" than system/current-time-millis), then
;;we're solid on the handling of time steps.

;;However, we also need to modify the handling of
;;the timers, which propogate events via javax.swing.timer
;;sending action events......

;;Activities use proot/createTimer to get their stuff...
;;so, we could override that.
;;Specifcially, we could create a timer that generates
;;action events based on another event, rather than a
;;side-effecting timer.

;;It seems like the scheme we want to follow is this:
;;override proot so that:
;; getGlobalTime uses an event-driven clock (probably an atom)
;; rather than system.
;; createTimer -> rather than creating a javax.swing.Timer,
;; return a subclassed timer that is synchronized with the
;; SAME global time as getGlobalTime...

;;so, we need to extend piccolo to use a couple of different
;;classes..
