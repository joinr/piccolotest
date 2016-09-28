;;Wrappers for the general PActivity classes in
;;piccolo2d.  One departure from the stock
;;piccolo2d stuff is the desire to have
;;activities respond to external events.
(ns piccolotest.activities
  (:require [piccolotest.interop :as interop])
  (:import [org.piccolo2d.activities PActivity PInterpolatingActivity
            PActivityScheduler]
                                        ;           [piccolotest.activities Timer PRoot]
           [org.piccolo2d PNode PRoot]
            [javax.swing SwingUtilities]))
        
(defn invoke-later! [^java.lang.Runnable f]
  (SwingUtilities/invokeLater f))

(defmacro do-scene
  "Ensures that any alterations of a live scene are handled on 
   the swing EDT thread, where appropriate.  For other backends this may
   be unncessary."
  [& body]
  `(invoke-later! (~'fn [] (do ~@body))))

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

(defn activity-info [^PActivity act]
  {:type (type act)
   :start (.getStartTime act)
   :duration (.getDuration act)
   :end (.getStopTime act)
   :next (.getNextStepTime act)
   :activity act})

;;this setups up functions that les us modify private
;;fields effeciently.  REALLY useful for setting up
;;PActivity objects....
(interop/expose-private-accessors org.piccolo2d.activities.PActivity
  nextStepTime)

;;the timeline wraps scheduler, on-tick, clock, changed?
;;and basically creates an alternate timeline not controlled
;;by typical activities.  So, if we register an activity
;;on the externally-driven timeline, then we're good to go.
;;We could even hack the scheduler to ignore the global
;;time passed in during process-activities.
(deftype timeline [^PActivityScheduler scheduler on-tick
                   clock
                   changed?]
  org.piccolo2d.PRoot$InputSource
  (^void processInput [this]
    (when @changed?
      (let [t (long @clock)
            _ (reset! changed? false)]
        (do (.processActivities scheduler t)
            (when on-tick (on-tick t))
            ))))
  clojure.lang.ISeq
  (seq [obj] (map activity-info (.getActivitiesReference scheduler)))
  clojure.lang.IDeref
  (deref [this] {:scheduler scheduler
                 :on-tick on-tick
                 :clock clock
                 :changed? changed?})
  )

;;we also want to register some side-effecting ops
;;the timeline listens to clock changes.
;;changes trigger process-input calls to root
;;(on the edt thread)
;;the timeline is an input-source, that when polled,
;;if changes are pending, we need to process the
;;registered activities with the new time.
(defn ->timeline
  ([^PRoot root clock on-tick]
   (let [changed? (atom false)
         _        (add-watch clock :timeline
                             (fn [r k old new]
                               (when (not (== old new))
                                 (reset! changed? true)
                                 (do-scene
                                  (.processInputs root))
                                 )))
        tm   (timeline.  (PActivityScheduler. root)
                         on-tick
                         clock
                         changed?)]
     (.addInputSource root tm)
     tm))
  ([root clock] (->timeline root clock nil)))

(defn ^PActivityScheduler get-scheduler [^PActivity activity]
  (.getActivityScheduler activity))

(defn activities [^timeline tl]
  (.getActivitiesReference ^PActivityScheduler (.scheduler tl)))

(defn ^PActivity set-start-time [^PActivity a t]
  (do (.setStartTime a (long t))
      (set-nextStepTime a (long t))
      a))
                
(defn on-timeline!
  "Schedule activity on tl.  If activity already exists on a 
   schedule, it will be removed, effectively 'swapping' to another
   timeline."
  [^PActivity activity ^timeline tl]
  (let [^PActivityScheduler sched  (get-scheduler activity)
        ^PActivityScheduler target (.scheduler tl)
        target-time @(.clock tl)
        ]
    (do (when (not (identical? sched target)) 
          (do (when sched (.removeActivity sched activity))
              (set-start-time activity target-time)
              (.addActivity target activity)))
        activity)))

;;now we need to define ways to override the
;;default behavior for scheduling activities
;;on a node...so that the activity lives on
;;a different timeline.

;;PNode has methods for scheduling
;;default activities...

;;AnimateToTransform, etc.
;;All of these use PNode/addActivity
;;which references the node's PRoot.
;;This calls PRoot's addActivity.  Note:
;;the individual methods also return the
;;scheduled activity as output...

;;So, the train of consequences (megadeth reference)
;;is this:
;;  If we control how we add activities to the PRoot
;;  we can tap into the default behavior of PNode's
;;  animation interface, and control the
;;  timeline that feeds steps to the activities.

;;  Currently, PRoot nodes have an addActivity
;;  method that controls "which" ActivitySchedule
;;  the activity is added to.
;;  The default is the System/time driven
;;  scheduler.
;;  We want to redirect the activity so that
;;  it's placed on our asynchronous scheduler...
;;  which hooks into the PRoot rendering
;;  process via another PInput source...

;;Options:
;;-  Subclass PRoot and override addActivity (least damaging)
;;-  Subclass PNode and override addActvitiy (far-reaching
;;-  consequences, since lots of things depend on PNode).
;;-  Use existing methods for addActivity, but manually
;;   move the activity from the scheduler to the async
;;   scheduler (may cost us performance....but not untenable)
;;   Activities are stored in an arraylist....so we could
;;   and-and-remove pretty easily.
;;-  Temporarily set the node's PRoot to nil, "fooling"
;;   piccolo into seeing the node as detached...
;;   This requires us to remove the node's parent, and
;;   reset it afterward.  Again, costs us performance
;;   in terms of property changes and triggering
;;   possible bounds updates.

;;Cleanest is to override PRoot....

;;Simplest is to manually remove the activity...
;;We'll go simple for now and see if there are any
;;implications...

;;So we need an API to wrap the process of animating
;;a node, either synchronously with global time,
;;or asynchronously with respect to an alternate,
;;event-driven timeline.

;;Complex Activities
;;==================
;;Can we define an activity chain or something?
;;We can possibly define new activities via
;;inheritance....

;;alternately, we could define our own functional
;;interface for scheduling activities via process
;;input....this may actually be faster.

;;Rather than using piccolo2d's api strictly...
;;we could tap into libraries like freactive...
;;and use reactions/ratoms to compose
;;animated values over time, again using our
;;clock to control the animation.

;;we can use PActivity/startAfter to script activities...
;;that's the only "combinator" between the activities...
;;All it's doing is altering the starttime of
;;the remaining activities...
;;note: there are possibly opportunities for
;;parallelism here, might investigate further.

(comment ;testing
  ;;mucking with the timeline
  (def tm (act/->timeline (.getRoot @canvas) clock (fn [t] (println [:time t]))))  
  )
              
;;we want to develop our own timeline...
;;the timeline, then, should be an
;;option for registering activities.


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

(comment 
(defn async-activities [time-source ^PRoot proot]
  (let [sched          (PActivityScheduler.  proot)
        current-time   (atom @time-source)        
        ]
    ))
)

