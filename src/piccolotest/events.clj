;;Namespace for defining bindings for piccolo input
;;processing via events.  Specifically, we want
;;to ease the definition of event handlers and
;;listeners and make it easy to define event
;;streams of PInput events (piccolo input).
(ns piccolotest.events
  (:import
   [org.piccolo2d         PCanvas PLayer PNode PRoot POffscreenCanvas PCamera]
   [org.piccolo2d.event   PBasicInputEventHandler PDragEventHandler
                          PInputEvent PInputEventFilter PInputEventListener]
   [org.piccolo2d.extras.event
    PSelectionEventHandler
    PNotification
    PNotificationCenter]
   [java.awt.event   InputEvent MouseEvent KeyEvent FocusEvent MouseWheelEvent]
   [java.awt.geom   Point2D AffineTransform]))

;;it'd be useful to bind node properties to input...
;;things like channels.

;;we could split this up into channels, but we'll keep with
;;piccolo conventions for now...It'd be nice to dispace
;;events based on type though.

(defn event-listener? [obj]
  (instance? org.piccolo2d.event.PInputEventListener obj))

;;We can define a higher-kinded way to connect our model with
;;the view...
;;If we tap into the event system, then we can map model events
;;into piccolo events....
;;If we relate entities to corresponding piccolo nodes, then
;;when an entity-related event happens, the nodes are notified.
;;Nodes subscribe based on interest...
;;SO...
(def event-types
  {'FocusEvent/FOCUS_GAINED FocusEvent/FOCUS_GAINED,
   'FocusEvent/FOCUS_LOST FocusEvent/FOCUS_LOST,
   'MouseWheelEvent/WHEEL_BLOCK_SCROLL MouseWheelEvent/WHEEL_BLOCK_SCROLL,
   'MouseWheelEvent/WHEEL_UNIT_SCROLL MouseWheelEvent/WHEEL_UNIT_SCROLL,
   'KeyEvent/KEY_RELEASED KeyEvent/KEY_RELEASED,
   'MouseEvent/MOUSE_MOVED MouseEvent/MOUSE_MOVED,
   'MouseEvent/MOUSE_EXITED MouseEvent/MOUSE_EXITED,
   'KeyEvent/KEY_PRESSED KeyEvent/KEY_PRESSED,
   'MouseEvent/MOUSE_CLICKED MouseEvent/MOUSE_CLICKED,
   'MouseEvent/MOUSE_ENTERED MouseEvent/MOUSE_ENTERED,
   'MouseEvent/MOUSE_RELEASED MouseEvent/MOUSE_RELEASED,
   'MouseEvent/MOUSE_DRAGGED MouseEvent/MOUSE_DRAGGED,
   'MouseEvent/MOUSE_PRESSED MouseEvent/MOUSE_PRESSED,
   'KeyEvent/KEY_TYPED KeyEvent/KEY_TYPED})

(def event-masks
  {:mouseEntered 504,
   :keyPressed 401,
   :focusLost 1005,
   :wheelUnitScroll 0,
   :mouseDragged 506,
   :wheelBlockScroll 1,
   :focusGained 1004,
   :keyReleased 402,
   :mouseClicked 500,
   :mouseReleased 502,
   :mousePressed 501,
   :mouseMoved 503,
   :keyTyped 400,
   :mouseExited 505})

(defmacro case-event-type [pred & cases]
  (let [tl (when (odd? (count cases)) (last cases))]
  `(case ~pred
     ~@(mapcat (fn [[s expr]]
                 [(or (get event-types s) (throw (Exception. (str [:unknown-eventtype s]))))
                  expr])
               (partition 2 cases))
     ~@tl)))

(defn make-filter [es]
  (let [mask  (reduce bit-or (map event-masks es))
        ;_ (println [:mask mask])
        ]
    (fn [event-type]
      (do ;(println [:et event-type])
          (pos? (bit-and mask (int event-type)))))))

(defn null-event [e]  nil)

(defn event-processor [& {:keys [keyPressed
                                 keyReleased
                                 keyTyped
                                 mouseClicked
                                 mouseDragged
                                 mouseEntered
                                 mouseExited
                                 mouseMoved
                                 mousePressed
                                 mouseReleased
                                 mouseWheelRotated
                                 mouseWheelRotatedByBlock
                                 keyboardFocusGained
                                 keyboardFocusLost
                                 event-filter]
                          :or {keyPressed null-event
                               keyReleased null-event
                               keyTyped null-event
                               mouseClicked null-event
                               mouseDragged null-event
                               mouseEntered null-event
                               mouseExited null-event
                               mouseMoved null-event
                               mousePressed null-event
                               mouseReleased null-event
                               mouseWheelRotated null-event
                               mouseWheelRotatedByBlock null-event
                               keyboardFocusGained null-event
                               keyboardFocusLost null-event}                               
                          :as m}]
  (let [known-events (keys (dissoc m :event-filter))
        efilter      (make-filter known-events)
        process?     (or event-filter (fn [_ event-type]
                                        (efilter event-type)))]
    (reify org.piccolo2d.event.PInputEventListener        
      (^void processEvent  [this ^PInputEvent event ^int event-type]
          (case-event-type  event-type
            KeyEvent/KEY_PRESSED                 (keyPressed    event)                
            KeyEvent/KEY_RELEASED                (keyReleased   event);                
            KeyEvent/KEY_TYPED                   (keyTyped      event);                
            MouseEvent/MOUSE_CLICKED             (mouseClicked  event);
            MouseEvent/MOUSE_DRAGGED             (mouseDragged  event);
            MouseEvent/MOUSE_ENTERED             (mouseEntered  event)
            MouseEvent/MOUSE_EXITED              (mouseExited   event);
            MouseEvent/MOUSE_MOVED               (mouseMoved    event);
            MouseEvent/MOUSE_PRESSED             (mousePressed  event);
            MouseEvent/MOUSE_RELEASED            (mouseReleased event);
            MouseWheelEvent/WHEEL_UNIT_SCROLL    (mouseWheelRotated event);
            MouseWheelEvent/WHEEL_BLOCK_SCROLL   (mouseWheelRotatedByBlock event);
            FocusEvent/FOCUS_GAINED              (keyboardFocusGained event);
            FocusEvent/FOCUS_LOST                (keyboardFocusLost event)))
      clojure.lang.IFn
      (invoke [this e t] (.processEvent this e (int t))))))
;;note: we could change this to an input-channel form...
;;or use the spork event observer utils to do something
;;similar.

;;PInputEvents basically take a normal swing InputEvent
;;and decorate it with additional information (i.e. node,
;;pickpath, etc.)



;(defn event-filter [])

;;I'd like to be able to define "selecting" things.
;;When entities are selected, they should be highlighted
;;somehow and tracked as a selection.  Note: we already have
;;an implementation of selection as a special handler.
;;It'd be nice to implement a modal-handler as well,
;;where mode is set depending on key modifiers...
;; (defn on-input
;;   ([^PNode nd f]
;;    (let [eh (if (listener? f) f
;;               (processEvent [this event t]
;;                             (f event t)))]
;;      (doto nd (.addInputEventListener ^PInputEventListener eh)))))

