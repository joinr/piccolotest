;;Wrappers for the general PActivity classes in
;;piccolo2d.  One departure from the stock
;;piccolo2d stuff is the desire to have
;;activities respond to external events.
(ns piccolotest.activities
  ;(:require [piccolotest.activities [Timer :as tmr]])
  (:import [org.piccolo2d.activities PActivity PInterpolatingActivity
            PActivityScheduler]
                                        ;           [piccolotest.activities Timer PRoot]
           [org.piccolo2d PNode PRoot]
           ))
        

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

;;So PRoot and Timer have been extended.
;;What now?
;;Are our problems solved?
;;Another observation: PActivities are scheduled
;;via the PActivityScheduler.

;;Note:
;;We can also just stick a surrogate on here...
;;Basically, have an ever-present activity that
;;provides a thread-safe facade for our non-piccolo
;;activities (i.e. clojure stuff).

;;From there, we have a hook into the input...
;;Then, we just use this surrogate to
;;schedule activities.  It never ends.

;;I like this solution mo betta.
;;Since we already do processInputs every time...

;;Activities are processed by taking steps, a step is
;;given at the current-time.
;;Current-time is processed in frame_interval steps,
;;activated via the activity-timer.
;;This means activities are synchronized with the
;;fram_interval based timer....

;;Control Flow
;;============
;;Activity-timer starts ticking every frame_interval...
;;  ticks queue up actionPerformed events on edt for Proot.

;;  (Swing or somebody) calls processInputs on edt for Proot...
;;  processInputs runs activities with the current-time (this is
;;  very similar to a time-stepped simulation).

;;  Note: the delta between current-time and the step-time may be
;;  less than the default_step, if there are user inputs in the interim,
;;  which cause updating.

;;  From here, activityScheduler in PRoot steps through each activity
;;  and calls processStep.
;;    This is more of less dispatching updates to each thread, if the
;;    activities are viewed as logical threads.

;;  After each activity has the chance to do its side-effecting thing..
;;  typically transforming nodes, then control returns to the
;;  proot.  "Damage" is calculated, fixed, and repainted.

;;  Thus ends the rendering cycle...

;;  Notice how control and view are pretty deeply intertwined here.
;;  The good new is that we can hack control flow and ensure our
;;  stuff runs on the EDT (thus preventing wierd errors like we're
;;  currently getting) by having a permanent activity installed
;;  that always steps....
;;  From there, we can either directly invoke or port the existing
;;  activities from piccolo and schedule them accordingly (i.e.
;;  animateToTransfrom and friends).  This ought to make life
;;  much easier, and still retain compatibility with the underlying
;;  class infrastructure.

;;  Note: InputSources
;;  Another option for doing stuff is to use the PRoot.InputSource
;;  class.  In PRoot, prior to processing activities, we have
;;  a hooksite for processing other InputSources.  This is
;;  simple, since all we have to do is reify a class that
;;  implements this method. We could have a piccolo
;;  command channel, for instance.
;;  This sidesteps the entire problem...
;;  We just add a reified inputsource via addInputSource
;;  and we're golden.
;;  The input source could be a simple command queue of stuff.

;;  So there are two options for implementing transforms...
;;  Use PActivities (native piccolo stuff), or
;;  implement an async input via InputSource, add the InputSource
;;  to the PRoot.
;;  This is, more or less what we're doing now anyways with the animation.
;;  We have an external input source that handles changes to the
;;  piccolo scene.   Every frame, we update the global time
;;  according to some step(s).  From there we update the state, which
;;  changes the scene (for the rendering system), and then we end up
;;  immediately invalidating the nodes.  Rather than queuing this
;;  as input on the piccolo graph, we end up
;;  doing it immediately, possibly during re-rendering in piccolo.
;;  This leads to concurrent modification errors, and all the
;;  nastiness that entails.

;;  If we formalize this, then we have a hook into the offscreen
;;  canvas as well (since it supports PRoot and such).


;;InputSource Implementation
;;==========================
;;PRoot has an innerclass, acccessible at
;;org.piccolo2d.PRoot$InputSource 

(defn ->input-source [process]
  (reify org.piccolo2d.PRoot$InputSource
    (^void processInput [this]
      (process))))

;;say we want to have an asynchronous transform event...
;;Can we have an activity scheduler that simply lives
;;on another timeline?
;;YES!
;;So, we create an activity scheduler, and wrap it as
;;another input source....
;;If time hasn't changed according to its time-source, then
;;we simply pass.
;;If time has changed, then we process its activities.
;;Thus, all we have to do to transform the node and
;;stuff is to schedule the canonical activities with
;;the async activity scheduler.

(defn async-activities [time-source ^PRoot proot]
  (let [sched          (PActivityScheduler.  proot)
        current-time   (atom @time-source)        
        ]
    ))
