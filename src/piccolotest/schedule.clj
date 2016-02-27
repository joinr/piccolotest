;;Currently obsolete namespace for scheduling activities.
;;Now we just use invokLater to schedule changes to the
;;scene graph on the EDT thread.  
(ns piccolotest.schedule)
;;We wrap the activity delegate schedule activities on the event dispatch
;;thread, so piccolo will properly schedule changes to nodes.  In particular,
;;when we schedule changes to the structure of the graph, we end up
;;with possible errors due to piccol being in the middle of painting.
;;In essence, we have to schedule nodes to be dropped at piccolo's convenience.
(defn ^org.piccolo2d.activities.PActivity$PActivityDelegate
  ->activity-delegate [& {:keys [start step finish]}]
  (reify org.piccolo2d.activities.PActivity$PActivityDelegate
    (^void activityStarted [this ^PActivity activity]
      (start activity))
    (^void activityStepped [this ^PActivity activity]
      (step activity))
    (^void activityFinished [this ^PActivity activity]
      (finish activity))))

(defn ^PActivity ->activity
  [& {:keys [duration step-rate start-time start finish step]
      :or {duration -1  ;infinite
           step-rate 1 ;60 fps
           start (fn [_] nil)
           step (fn [_] nil)
           finish (fn [_] nil)}
           }]
  (let [start-time (or start-time (System/currentTimeMillis))
        pa (PActivity. (long duration) (long step-rate) (long start-time))
        del (->activity-delegate :start start :finish finish :step step)]
    (doto pa (.setDelegate del)))) 

;;schedule an activity...
;;should this be with root?
(defn ^PNode add-activity! [^PNode node ^PActivity act]
  (if-let [res (.addActivity node act)]
    node
    (throw (Exception. (str "Tried to add an activity to " node " with no root")))))

(defn get-schedule [^PNode nd]
  (if-let [^PRoot rt (.getRoot nd)]
    (if-let [s (get (node-meta ^PNode rt) :schedule)]
      s
      (let [schedule (ConcurrentLinkedQueue.) ;we're allowing multiple threads to schedule.
            _       (with-node-meta rt {:schedule schedule})
            ;;if the schedule doesn't exist, we'll add a perpetual activity that checks
            ;;the schedule for thunks, and flushes the schedule by executing thunks between frames.
            _       (add-activity! rt  
                       (->activity :step
                           (fn [_]
                             (loop []
                               (when-let [f (.poll schedule)]
                                 (do (f)
                                     (recur))))))) 
            ]
        schedule))
    (throw (Exception. (str "Node is unrooted, cannot be scheduled." nd)))))

(defn push-schedule! [^ConcurrentLinkedQueue s o]
  (doto s (.add o)))

(defmacro schedule!
  "Schedule body to run in between rendering the next frame.  Basically, 
   defer execution until piccolo is in between frames, i.e. nothing is 
   rendering.  This allows us to concurrently serialize changes to scene 
   graph without generating inconsistencies and thus errors (like 
   arraylist index errors while trying to iterate over nonexistent 
   children due to drops) "
  [nd & body]
  (let [nd (with-meta nd {:tag 'org.piccolo2d.PNode})]
    `(let [thunk# (fn [] ~@body)
           sched# (get-schedule ~nd)]
       (do (push-schedule! sched# thunk#)
           ~nd))))


(comment 
(dotimes [i 1000]
  (let [[c r] [(rand-int 28) (rand-int 60)]]
    (-> (get-cell tbl r c)
        (set-paint! (rand-nth [:red :blue :orange :yellow :green]) )
        (invalidate!))
    (Thread/sleep 16)))
)
