(ns piccolotest.sample
  (:require [spork.cljgui.components.swing :as gui]
            [spork.graphics2d.canvas :as canvas]
            [spork.geometry.shapes :as shapes]
            [spork.graphics2d.swing :as swing]
            [spork.graphics2d.font :as font]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]
            [piccolotest [events :as events] [properties :as props] [activities :as acts]])
  (:import
   [org.piccolo2d         PCanvas PLayer PNode PRoot POffscreenCanvas PCamera]
   [org.piccolo2d.event   PBasicInputEventHandler PDragEventHandler
                          PInputEvent PInputEventFilter]
   [org.piccolo2d.nodes   PPath PImage PText PShape]
   [org.piccolo2d.util   PBounds
                         PAffineTransform]
   [org.piccolo2d.extras.event
    PSelectionEventHandler
    PNotification
    PNotificationCenter
    PNavigationEventHandler]
   [org.piccolo2d.activities    PActivity]
   [org.piccolo2d.extras.pswing PSwing PSwingCanvas]
   [org.piccolo2d.extras.swing SwingLayoutNode]
   [org.piccolo2d.extras.swing.SwingLayoutNode.Anchor]
   [org.piccolo2d.extras.nodes PNodeCache PLine]
   [java.awt Color Dimension Graphics2D  GridBagConstraints GridBagLayout BorderLayout FlowLayout 
    GridLayout  Component Insets]
   [java.awt Paint]
   [java.awt.event   InputEvent MouseEvent]
   [java.awt.geom   Point2D AffineTransform]
   [javax.swing     JFrame JPanel Box BoxLayout] 
   [java.util  ArrayList Random]
   [java.util.concurrent ConcurrentLinkedQueue]
   [javax.swing SwingUtilities]))

;;Useful protocol 

;;nodes communicate via a node channel
(def node-channel (chan (a/dropping-buffer 100)))
(def ^:dynamic *cartesian* true)

(defn notify! [msg]
  (go (a/put! node-channel msg)))

(defn notify!! [msg]
  (>!! node-channel msg))

(defn invoke-later! [^java.lang.Runnable f]
  (SwingUtilities/invokeLater f))

(defmacro do-scene
  "Ensures that any alterations of a live scene are handled on 
   the swing EDT thread, where appropriate.  For other backends this may
   be unncessary."
  [& body]
  `(invoke-later! (~'fn [] (do ~@body))))

(defn do-input! [^PCanvas cnv]
  (.processInputs (.getRoot cnv)))

;;These may be giving us grief, since we're not on the edt...
;;note: we could queue up children for removal on the edt..
;;rather than spam the edt with tons of child removal requests,
;;we could queue them up.
(defn add-child!  [^PNode p ^PNode c] (doto p (.addChild c)))
(defn drop-child! [^PNode p ^PNode c] (doto p (.removeChild c)))

(defn ^java.util.ArrayList children! [^PNode p] (.getChildrenReference p))
(defn empty-list? [^java.util.ArrayList a]
  (.isEmpty a))

;;pops either the last or the first child....
;;for stack operations
(defn first-child! [^PNode p]
  (let [xs (children! p)]
    (when-not (empty-list? xs)
      (.get xs 0))))

(defn last-child!  [^PNode p]
  (let [xs (children! p)]
    (when-not (empty-list? xs)
      (.get xs (unchecked-dec
                (.size xs))))))

;;stack operations
(defn pop-child!  [^PNode p]
  (let [xs (children! p)]
    (if-not (empty-list? xs)
      (do   (.remove xs (unchecked-dec
                         (.size xs)))
            p)
      p)))

;;for queue operations
(defn poll-child! [^PNode p]
  (let [xs (children! p)]
    (if-not (empty-list? xs)
      (do   (.remove xs 0)
            p)
      p)))

(defprotocol MetaNode
  (node-meta [nd])
  (with-node-meta [nd m]))

(defn vary-node-meta [nd f & args]
  (with-node-meta nd
    (apply f (node-meta nd) args)))

(extend-type org.piccolo2d.PNode
  MetaNode
  (node-meta [obj]         (.getAttribute obj "meta"))
  (with-node-meta [obj m]  (.addAttribute obj "meta" m) obj))

(defn property-filter  [k pred]
  (fn property-filter [nd]
    (when-let [res (get (node-meta nd) k)]
      (pred res))))

;;the problem we're currently having is extending node-like functionality
;;to things defined earlier, namely things that extend the
;;spork.graphics2d.canvas.IShape protocol.

;;We can go the route that clojure goes with the ISeq protocol, having a
;;member function - seq - that we can use to initialize anything to a compatible
;;node.  The only thing is, we want to transitively say that "any" shape can
;;be viewed as a node....yet shapes are identified via the protocol.  Multimethods are
;;a (somewhat slow) option; we can just modify the function used to identify the method
;;dispatch, and from there, we go grow the implementations (i.e. the implementation is
;;"open").

;;Note:  we won't be creating "lots" of nodes, so the slowness of multimethods may not
;;be a big deal here (unlike other areas).
;;Another option is to use a factory function that delegates to a multimethod to create a
;;wrapped IPiccNode class (along with a dereffable object).  Clojure does this with the seq
;;function (seq is a constructor that makes - or wraps - things into an object that is compatible
;;or provides a seq'd view of the underlying thing.
;;as-node should probably be like this.

;;Another option is to explicitly extend as-node to shapes, or - more generally - to
;;have a single function that detects if the underlying obj extends IPiccNode, and then
;;delegates to as-node-, or if it's a shape (and only for shapes), creates a new node
;;from that.  For instance, I'd like to just shove a "shape" into a node and have it exist in
;;the hierarchy.  This makes things easier to compose.
(defprotocol IPiccNode
  (as-node [nd])
  (add-child [nd chld]))
  
;;We're currently not covering intersection in the spork shape library.
;;To make custom nodes, which have accurate picking, we'd have to do that.
;;If we don't care about picking, or we're okay with simply bounding boxes,
;;then using the existing simplistic bounding boxes works fine.  Otherwise,
;;we go into caching intersections and such (not necessarily difficult with
;;immutable, reified objects, but not exactly desirable either).

;; (defn as-node [obj]
;;   (let [cls (class obj)]
;;     (cond (extends? IPiccNode cls)      (as-node- obj)
;;           (extends? canvas/IShape cls)  (as-node- ( obj)
    
;;we should interface with the shapes protocol to allow them to be nodes.
;;they could either be custom nodes or images...
;;still determining a better way to do this.
;;Best way is to enforce shapes to have a more primitive shared interface for
;;custom pnodes, or at least able to be interpreted as pnodes.
;;Actually "best" way is to implement scene graph directly :(

(extend-protocol IPiccNode
  org.piccolo2d.PNode
  (as-node   [nd]       nd)
  (add-child [nd chld] (do (.addChild nd (as-node chld)) nd))
  org.piccolo2d.PLayer
  (as-node   [nd]       nd)
  (add-child [nd chld] (do (.addChild nd (as-node chld)) nd))
  org.piccolo2d.PCanvas
  (as-node   [nd]        (.getRoot ^PCanvas nd))
  (add-child [nd chld]   (do (.addChild (.getLayer nd) (as-node chld)) nd))
  org.piccolo2d.POffscreenCanvas
  (as-node   [nd]        (.getRoot ^POffscreenCanvas nd))
  (add-child [nd chld] (do (.addChild (.getLayer ^PCamera (.getCamera nd) 0) (as-node chld)) nd))  
  org.piccolo2d.extras.pswing.PSwingCanvas
  (as-node [nd]        (.getRootNode ^PSwingCanvas nd))
  (add-child [nd chld] (do (.addChild (.getLayer ^PSwingCanvas nd) (as-node chld)) nd))
  clojure.lang.PersistentVector
  (as-node [nd]        (reduce add-child (PNode.)  nd))
  (add-child [nd chld] (add-child (as-node nd) chld))
  clojure.lang.LazySeq
  (as-node [nd]        (reduce add-child (PNode.)  nd))
  (add-child [nd chld] (add-child (as-node nd) chld))
  javax.swing.JPanel
  (as-node [nd]   (PSwing. nd))
  (add-child [nd child] (add-child (PSwing. nd) child))
  javax.swing.JComponent
  (as-node [nd]   (PSwing. nd))
  (add-child [nd child] (add-child (PSwing. nd) child)))

(defn add-children [nd xs]
  (reduce add-child nd xs))

(defn leaf?     [nd]
  (pos? (.getChildrenCount ^PNode (as-node nd))))

(defn node-children [nd]
  (iterator-seq (.getChildrenIterator (as-node ^PNode nd))))

;;Search through the node meta data for ids
(defn find-node [id ^PNode root]
  (let [m (or (node-meta root) nil)]
    (if (= (get m :id) id) root
        (reduce (fn [acc nd]
                  (if (find-node id nd)
                    (reduced nd)
                    acc)) nil
                    (node-children root)))))

;;note: tree-seq flattens out our nodes in a depth-first order.
;;If we want to show containment, we need to walk the nodes iteratively.
(defn node-seq [^PNode root]
  (tree-seq leaf? node-children root))

(defn node-map [^PNode nd]
  (into {}
        (filter (fn [[l r]]
                  l)
                (for [nd (node-seq nd)]
                  [(node-meta nd) nd]))))

(defn node-parent [nd]
  (.getParent ^PNode (as-node nd)))

(defn node-root [nd]
  (.getRoot ^PNode (as-node nd)))

(defn ->cache [child]
  (let [c (PNodeCache.)
        ]
    (add-child c child)))

(defn ^PNode set-paint! [^PNode nd clr]
  (doto nd
    (.setPaint ^java.awt.Color (swing/get-gui-color clr))))

(defn ^PNode set-text-paint! [^PText nd clr]
  (doto nd
    (.setTextPaint ^java.awt.Color (swing/get-gui-color clr))))
(defn ^PText set-text! [^PText nd ^String txt]
  (doto nd
    (.setText txt)
    ))

(defn ^PNode set-font! [^PText nd fnt]
  (doto nd
    (.setFont (font/get-font fnt))))

(defn ^PBounds as-bounds [o]
  (cond (instance? org.piccolo2d.util.PBounds o) o
        (vector? o)
          (let [[x y w h] o]
            (PBounds. (double x) (double y) (double w) (double h)))
          :else (throw (Exception. (str [:cannot-make-bounds o])))))

(defn ^PBounds get-bounds [^PNode o] (.getBounds o)) 
(defn ^PBounds get-full-bounds [^PNode o] (.getFullBounds o)) 

(defn ^PNode set-bounds! [nd bnds]
  (doto nd (.setBounds (as-bounds bnds))))

(defn ^PNode invalidate! [^PNode nd]
  (doto nd
    (.invalidatePaint)))

;;so layers can act like groups.  They are also nodes...
(defn ->layer
  ([xs meta]
   (reduce (fn [^PLayer acc ^PNode n]
             (doto acc (add-child n)))
           (doto (PLayer.) (.addAttribute "meta" meta)) xs))
  ([xs] (->layer xs   {}))
  ([]   (->layer nil  {})))

(defn node? [nd]  (instance? org.piccolo2d.PNode nd))
(defn ->panel [^JPanel pnl]
  (if (node? pnl) pnl
      (doto (PSwing. pnl)
                                        ;(.setUseBufferedPainting true)
        )))
   
;;note: a canvas is a panel...
(defn ^PSwingCanvas ->swing-canvas
  ([pnl]
   (add-child (PSwingCanvas.) pnl))
  ([] (PSwingCanvas.)))

;;cribbed from putil, with the ability to
;;inject alternate components...
(defn ->basic-scene
  ([root]
   (let [layer (PLayer.)
         camera (PCamera.)]
     (do  (add-children root [camera layer])
          (doto camera (.addLayer layer)))))
  ([] (->basic-scene (PRoot.))))

(defn ^PCanvas ->canvas
  ([]  (PCanvas.))
  ([& nodes] (reduce add-child (PCanvas.) nodes)))

;;we should be able to transplant a camera to a new
;;root easily.  The layer doesn't change.  Just
;;transplant the root's children.  Should work fine.
(defn swap-root [canvas new-root]
  (let [cam      (.getCamera canvas)
        nodes    (node-children (.getRoot cam))
        new-root (add-children new-root nodes)]
    canvas))

(defn ^POffscreenCanvas ->offscreen-canvas
  ([w h]  (POffscreenCanvas. (int w) (int h)))
  ([w h & nodes] (reduce add-child (POffscreenCanvas. (int w) (int h)) nodes)))

;(defn ->layer  []  (PLayer.))

(defn ^double get-yscale [^PNode nd]
  (.getScaleY (.getTransformReference nd false)))
(defn ^double get-xscale [^PNode nd]
  (.getScaleX (.getTransformReference nd false)))
(defn cartesian? [^PNode nd]
  (neg? ^double (get-yscale nd)))

;;we should redefine this relative to cartesian coords.
(defn ^PNode translate!
  [^PNode nd ^double x ^double y]
   (doto nd (.translate x y)))

(defn ^PNode transform!
  [^PNode nd ^AffineTransform xform]
  (doto nd (.setTransform xform)))

(defn ^PNode set-visible!
  [^PNode nd vis]
   (doto nd (.setVisible (boolean vis))))

;;Animated Activities
;;===================

;;These are all based on basic activities, notably
;;the global time derived from proot's get-time, using
;;System/currentTimeMillis.
;;If we hack the global time in root, we can get parameteric,
;;event-based animations for free.  Also (alternatively), we
;;can extend pactivity to have event-driven durations or
;;non-standard views of global time.

;;Currently, we allow the definition of asynchronous
;;timelines (i.e. activity schedules that don't follow
;;the System/currentTime that piccolo uses by default
;;in the PRoot)

;;So, the default piccolo API will schedule activities
;;for the node, if the node has a root (with an
;;activity scheduler), then return the activity.

;;We'd like to communicate the intent to schedule
;;the activity elsewhere, by specifying an alternate
;;timeline.

(defn derive-timeline
  "Tries to find a timeline associated with the 
  root of nd, stored in the node properties under
  :default-timeline"
  ([nd clock nm]
   (let [rt (node-root nd)]
     (if-let [tl (get (node-meta rt) nm)]
       tl
       (let [tl (acts/->timeline rt clock)
             _  (vary-node-meta rt assoc nm tl)]
         tl))))
  ([nd clock] (derive-timeline nd  clock :default-timeline))
  ([nd] (derive-timeline nd (atom 0) :default-timeline)))

;;Synchronous activities (driven by SystemTime)
;;============================================
;;This actually can embiggen the node....
(defn animate-to-bounds! [nd x y w h duration]
  (do 
    (.animateToBounds ^PNode (as-node nd)
       (double x) (double y) (double w) (double h) (long duration))
    nd))

(defn animate-transform-to-bounds! [nd x y w h duration]
  (do 
    (.animateTransformToBounds ^PNode (as-node nd)
       (double x) (double y) (double w) (double h) (long duration))
    nd))

;;this parametrically animates all three properties...
(defn animate-to-position-scale-rotation! [nd x y scale theta duration]
  (do (.animateToPositionScaleRotation
       ^PNode (as-node nd) (double x) (double y) (double scale)
       (double theta) (long  duration))
      nd))

;;Asynchronous activities
;;Driven by a timeline arg...
(defn animate-to-bounds
  ([nd x y w h duration timeline]
   (let [act  (.animateToBounds ^PNode (as-node nd)
                                (double x) (double y)
                                (double w) (double h) (long duration))]
     (acts/on-timeline! act timeline) 
     nd))
  ([nd x y w h]
   (animate-to-bounds nd x y w h (derive-timeline nd))))

(defn animate-transform-to-bounds
  ([nd x y w h duration timeline]
   (let [act  (.animateTransformToBounds ^PNode (as-node nd)
                  (double x) (double y)
                  (double w) (double h) (long duration))]
     (acts/on-timeline! act timeline)        
     nd))
  ([nd x y w h duration]
   (animate-transform-to-bounds nd x y w h (derive-timeline nd))))

;;this parametrically animates all three properties...
(defn animate-to-position-scale-rotation
  ([nd x y scale theta duration timeline]
   (let [act (.animateToPositionScaleRotation ^PNode (as-node nd)
                (double x) (double y) (double scale)
                (double theta) (long  duration))]
     (acts/on-timeline! act timeline)
     nd))
  ([nd x y scale theta duration]
   (animate-to-position-scale-rotation
     nd x y scale theta duration (derive-timeline nd))))

(comment ;testing

  (def fr (->filled-rect :red 0 0 100 100))
  (render! fr)
  (def tl (derive-timeline fr))
  (def clock (:clock @tl))
  (animate-to-position-scale-rotation fr 100 1000 1.0 0 2000)

  ;;more complicated
  (defn big-test [& {:keys [dur] :or {dur 30000}}]
    (let [rects (for [i (range 1000)]
                  (->filled-rect (java.awt.Color. (int (rand-int 255)) (int (rand-int 255)) (int (rand-int 255)))
                                 (rand-int 1000)
                                 (rand-int 1000)
                                 20 20))
          ]
      (do (render! rects)
          (doseq [r rects]
            (animate-to-position-scale-rotation r (rand-int 1000) (rand-int 1000) 1.0 (/ Math/PI 2.0) dur))
          (let [clock (:clock @(derive-timeline (first rects)))]
            (dotimes [i (/ dur 20.0)]
              (swap! clock (fn [x] (unchecked-add x 20)))
              (Thread/sleep 20))))))

  ;;now we'd like to schedule multiple dependent activities....
  ;;this is like an activity chain


  )

;;getting the idea for reactive nodes....
;;nodes that are a function of time....

;;probably make this something else...
(defn ^PNode translate
  [^PNode nd ^double x ^double y]
   (doto nd (.translate x (- y))))

(defn ^PNode translate-to!
  ([^PNode nd ^double x ^double y]
   (do (.setGlobalTranslation nd (java.awt.geom.Point2D$Double. x y))
       nd))
  ([nd ^clojure.lang.PersistentVector xy] (translate-to! nd (.nth xy 0) (.nth xy 1)))) 

(defn ^PNode translate-by! [^PNode nd ^double x ^double y ]
  (let [trans (doto (AffineTransform.)
                    (.translate (double x) (double (if *cartesian* (- y) y))))]
    (doto nd (.setTransform trans))))

;;Imported from quilsample experiments.
;;This is more sophisticated than the animated-board, we may
;;want to retrofit the AB as a map-board.
(defn shift-node! [^org.piccolo2d.PNode nd ^double x ^double y]
  (do (translate nd x y) 
      (.invalidatePaint nd)
      nd))

(defn ^PNode scale!
  ([^PNode nd  xscale yscale]
   (let [xscale  (double xscale)
         yscale  (double yscale)
         trans (doto (AffineTransform.)
                 (.scale (double xscale) (double yscale)))]
     (doto nd (.transformBy trans))))
  ([^PNode nd ^double scale]
   (doto nd (.setScale scale))))

(defn ^PPath stroke! [^PPath nd ^java.awt.Stroke s]
  (doto nd
    (.setStroke s)))

(defn ^PPath stroke-paint! [^PPath nd  paint]
  (doto nd
    (.setStrokePaint (swing/get-gui-color paint))))

;;I think we want this to be 0.0 for the x coordinate, not 1.0....
(defn ^PNode uncartesian! [^PNode nd]
  (let [height (.getHeight nd)]
    (-> nd 
        (translate! 0.0 height) ;altered from 1.0
        (scale! 1.0 -1.0))))
;(defn ^PNode scale! [^PNode nd ^double x ^double y] (doto nd (.scale x y)))



;;It'd be nice to have a visual representation of the scene that we can mess with, explore, etc.
;;Something like smalltalk's object explorer...
;;this is really the scene-graph.
;; (defn node-tree [^PNode root]
;;   (if (leaf? root)
;;     (let [nm (node-meta nd)]
;;       {(or (:id nm) (str "leaf" )
;;        (clojure.lang.MapEntry. (node-meta nd) nd)})
;;     (let [nm (node-meta nd)]
;;       {(or (:id nm) :branch)
;;        (lazy-seq
;;         (map node-tree
          
;;   (tree-seq leaf?
;;             node-children root))  
        



;;Sticks the node to said camera by following the camera's
;;view transform.  Acts as a virtual child of the camera...
;;sticky node basically...
(defn ->stick-to-camera [nd ^PCamera cam]
  (let [old-scale (volatile! (.getViewScale cam))
        rescale! (fn rescale! [nd newscale]
                   (if (= @old-scale newscale)
                     nd
                     (do                                  
                       (vreset! old-scale newscale)
                       (scale! nd (/ 1.0 newscale)))))
        stick! (fn stick! []
                 (let [bnds (.getViewBounds cam)
                       x (.getX bnds)
                       y (.getY bnds)
                       new-scale (.getViewScale cam)]
                   (-> nd
                       (translate-to! x y)
                       (rescale! new-scale);(/ 1.0 new-scale))
                       )))
        _   (stick!)]       
    (doto cam
      (.addPropertyChangeListener PCamera/PROPERTY_VIEW_TRANSFORM
          (props/property-listener
           {PCamera/PROPERTY_VIEW_TRANSFORM
            (fn [old new]
              (stick!))})))))


;;maybe rename this piccpanel?
;;this is akin to the basic scene-graph
;;configuration.
(defprotocol IPiccScene
  (base-layer    [scn])
  (sticky-layer  [scn])
  (active-camera [scn]))

(defn add-sticky [scn nd]
  (add-child (sticky-layer scn) nd))

;;Defines implementations for standard scene conventions, with a sticky
;;layer and a base (or default) piccolo2d layer.  Things in the sticky
;;layer will automatically keep pace with the camera.
(extend-protocol IPiccScene
  org.piccolo2d.PCanvas
  (base-layer    [scn] (.getLayer ^PCamera (active-camera scn) 0))
  (sticky-layer  [scn] (if-let [l (get :sticky-layer (node-meta (.getRoot scn)))]
                         l
                         (let [^PLayer  l (-> (->layer)
                                              (with-node-meta {:id :sticky-layer}))
                               _  (->  (add-child (.getRoot ^PCanvas scn)
                                                  l)
                                       (with-node-meta {:sticky-layer l}))
                               ^PCamera cam (active-camera scn)
                               _ (->stick-to-camera l cam)
                               _ (.addLayer cam l)]
                           l)))
  (active-camera [scn] (.getCamera ^PCanvas scn)) 
  org.piccolo2d.POffscreenCanvas
  (base-layer    [scn] (.getLayer ^PCamera (active-camera scn) 0))
  (sticky-layer  [scn] (if-let [l (get :sticky-layer
                                       (node-meta
                                        (.getRoot ^POffscreenCanvas scn)))]
                         l
                         (let [^PLayer  l (-> (->layer)
                                              (with-node-meta {:id :sticky-layer}))
                               _  (->  (add-child (.getRoot ^POffscreenCanvas scn)
                                                  l)
                                       (with-node-meta {:sticky-layer l}))
                               ^PCamera cam (active-camera scn)
                               _ (->stick-to-camera l cam)
                               _ (.addLayer cam l)]
                           l)))
  (active-camera [scn] (.getCamera ^POffscreenCanvas scn)) 
  org.piccolo2d.extras.pswing.PSwingCanvas
  (base-layer    [scn] (.getLayer ^PCamera (active-camera scn) 0))
  (sticky-layer  [scn] (if-let [l (get :sticky-layer
                                       (node-meta
                                        (.getRoot ^PSwingCanvas scn)))]
                         l
                         (let [^PLayer  l (-> (->layer)
                                              (with-node-meta {:id :sticky-layer}))
                               _  (->  (add-child (.getRoot ^PSwingCanvas scn)
                                                  l)
                                       (with-node-meta {:sticky-layer l}))
                               ^PCamera cam (active-camera scn)
                               _ (->stick-to-camera l cam)
                               _ (.addLayer cam l)]
                           l)))
  (active-camera [scn] (.getCamera ^PSwingCanvas scn)))

;;cameras give us additional views...
(defn ^org.piccolo2d.PCamera ->camera []
  (org.piccolo2d.PCamera.))

;;a sub-scene creates three things:
;;a camera
;;a layer
;;
(defn ->sub-scene [])

;;we'd like to create multiple "panes" or picture-in-picture
;;views....
(defn ->sub-view  [])

;;we should be able to create a simple object explorer here...
;;Since we've coded a bunch of stuff...
;;We should be able to show a treemap or something like it...

(defn ^PNode ->rect
  ([color x y w h meta]
   (->
     (doto
       (PPath/createRectangle (double x) (double y) (double w) (double h))
       (.setStrokePaint (swing/get-gui-color color))
       (.setPaint nil))
     (with-node-meta meta)))
  ([color x y w h] (->rect color x y w h {})))

(defn ^PNode ->filled-rect
  ([color x y w h meta]
   (->
     (doto
       (PPath/createRectangle (double x) (double y) (double w) (double h))
       (.setPaint (swing/get-gui-color color))
       (.setStroke nil))
     (with-node-meta meta)))
  ([color x y w h] (->filled-rect color x y w h {})))

(defn ^PNode ->line
  ([color x y x2 y2 meta]
   (->
     (doto
       (PPath/createLine (double x) (double y) (double x2) (double y2))
       (.setStrokePaint (swing/get-gui-color color)))
     (with-node-meta meta)))
  ([color x y x2 y2] (->line color x y x2 y2 {})))

(defn ^PNode ->arc
  ([color x y width height start extent meta]
   (->
     (doto
       (PPath/createArc (double x) (double y) (double width) (double height) (double start) (double extent) java.awt.geom.Arc2D/OPEN)
       (.setStrokePaint (swing/get-gui-color color)))
     (with-node-meta meta)))
  ([color x y width height start extent] (->arc color x y width height start extent {})))

(defn clear-background! [^PNode nd]
  (.setPaint (java.awt.Color. 1 0 0 100)))


(defn ^PNode ->quadCurve
  ([color x y ctrlx ctrly x2 y2 meta]
   (->
     (doto
       (PPath/createQuadCurve (double x) (double y) (double ctrlx) (double ctrly) (double x2) (double y2))
       (.setStrokePaint (swing/get-gui-color color))
       (.setPaint (java.awt.Color. 1 0 0 0))
     (with-node-meta meta))))
  ([color x y ctrlx ctrly x2 y2 ] (->quadCurve color x y ctrlx ctrly x2 y2  {})))


(defn flatten-path [^PPath p flatness]
  (let [points (double-array 6)
        p (java.awt.geom.FlatteningPathIterator. (.getPathIterator (.getPath p) (java.awt.geom.AffineTransform.))
                                                 flatness
                                                 )]
    (loop [acc []]
      (if (.isDone p)
        acc
        (do (.currentSegment p points)
            (.next p)
            (recur (conj acc  [(aget points 0)
                               (aget points 1)
                              ])))))))

(defn ->marked-path [^PPath p]
  [p
   (mapv (fn [[x y]]
           (->rect :black x y 1 1))
         (flatten-path p 1))])
             

(defn ^PNode ->orientedCurve
  ([color x y x2 y2 meta]
   (let [dx (- x2 x)
         dy (- y2 y)
         mpx (+ x (/ dx 2.0))
         mpy (+ y (/ dy 2.0))
         [nx ny] (if  (< y2 y)
                   [(/ dx 2.0)
                    (/ (- dy) 2.0)]
                   [ (/ (- dx) 2.0)
                     (/ dy 2.0)])              
         ctrlx (+ mpx nx)
         ctrly (+ mpy ny)
         ]
     (->quadCurve color x y ctrlx ctrly x2 y2 meta)))
  ([color x y x2 y2] (->orientedCurve color x y x2 y2 {})))

(defn ^PNode ->circle
  ([color x y w h meta]
   (->  (doto
          (PPath/createEllipse (double x) (double y) (double w) (double h))
          (.setPaint (swing/get-gui-color color)))
        (with-node-meta meta)))
  ([color x y w h] (->circle color x y w h {})))

;;Creates a grouped node with simple grid-lines overlaying
;;the node.  Probably should create a simple ->grid node
;;type.  This will subdivide the node boundaries into
;;a 10x10 grid
(defn ->grid-lines
  [^org.piccolo2d.PNode nd]
   (let [bounds (.getFullBounds nd)
         height (.getHeight bounds)
         width  (.getWidth bounds)
         x      (.getX bounds)
         y      (.getY bounds)
         vstep  (/ height 10.0)
         hstep  (/ width  10.0)]
     (into [nd]
           (concat 
            (for [n (range 11)]
              (->line :black x (+ y (* n vstep)) width (+ y (* n vstep))))
            (for [n (range 11)]
              (->line :black (+ x (* n hstep)) y  (+ x (* n hstep)) height ))))))

;;Adds a colored rectangle as a background for the node.
;;Should this be an image?  Should we just alter the node's
;;color property?
(defn ->background [clr nd]
  (let [bounds (.getFullBounds nd)
        height (.getHeight bounds)
        width  (.getWidth bounds)
        x      (.getX bounds)
        y      (.getY bounds)]
    [(->filled-rect clr x y width height)
     nd]))

;;images are shape stacks, so we can draw onto them.
(extend-type org.piccolo2d.nodes.PImage
  spork.graphics2d.canvas/IShapeStack
  (push-shape [obj shp] (canvas/push-shape (.getImage obj) shp))
  (pop-shape [obj] obj)
  spork.graphics2d.canvas/IWipeable
  (wipe [obj] (canvas/wipe (.getImage obj))))
;;We can annotate text and images to proxy PNode....
;;Or we we can just accept that we'll always have them flipped upside down.
;;When we go to render the text, they'll come out fine.  Actually, the
;;bounds won't be affected for text or images, so...picking shouldn't be affected.
;;We can just pre-scale the images and text to be flipped.
;;As long as they have no children, we should be alright.
(defn ^PNode ->image
  ([source meta] (->  (PImage. ^java.awt.Image (spork.graphics2d.canvas/as-buffered-image source :buffered-image))
                      (with-node-meta meta)
                      (uncartesian!)))
  ([source] (->image source {})))



;;given a shapestack...
;;we can get a sketching surface that's compatible as both a
;;PNode and something we can sketch onto without retaining
;;info (perfect for our trails layer).
(defn ^PImage ->sketch
  ([w h] (->sketch [] w h))
  ([shps w h] (let [ss       (shapes/->rec shps w h)
                    buf      (:buffer ss)
                    my-image (proxy [PImage spork.graphics2d.canvas.IShapeStack spork.graphics2d.canvas.IWipeable]
                                 [^java.awt.Image (canvas/as-buffered-image buf :buffered-image)]                          
                               (push_shape [shp] (do (canvas/push-shape buf shp)
                                                     (proxy-super invalidatePaint)
                                                     this))
                               (pop_shape  []     this)
                               (wipe       [] (canvas/wipe ss) this))]
           my-image)))
       
;;rewrite using our node transforms.
(defn ^PNode ->shelf
  [& nodes]
  (let [nd 
        (proxy [org.piccolo2d.PNode] []
          (layoutChildren [] 
            (reduce (fn [^double xoffset ^PNode nd]
                      (let [w (.getWidth (.getFullBoundsReference nd))]              
                        (.setOffset nd (- xoffset (.getX nd)) 0.0)
                        (+ xoffset w)))
                    0.0
                    (iterator-seq (.getChildrenIterator this)))
            (proxy-super layoutChildren)))]
    (doseq [n nodes]
      (add-child nd (as-node n)))
    nd))

;;generic pack functions....
;;try to fit nodes into a geometric space....
;;Note: we can probably do this via triangulation...
;;I'm not going that far....I just need something quick and dirty.

;;lets define other versions...
;;like a node that packs items into a geometric area.
(defn ^PNode ->container
  [& nodes]
  (let [nd 
        (proxy [org.piccolo2d.PNode] []
          (layoutChildren [] 
            (reduce (fn [^double xoffset ^PNode nd]
                      (let [w (.getWidth (.getFullBoundsReference nd))]              
                        (.setOffset nd (- xoffset (.getX nd)) 0.0)
                        (+ xoffset w)))
                    0.0
                    (iterator-seq (.getChildrenIterator this)))
            (proxy-super layoutChildren)))]
    (doseq [n nodes]
      (add-child nd (as-node n)))
    nd))


(defn ^PNode ->spaced-shelf
  [spacing & nodes]
  (let [nd 
        (proxy [org.piccolo2d.PNode] []
          (layoutChildren [] 
            (reduce (fn [^double xoffset ^PNode nd]
                      (let [w (.getWidth (.getFullBoundsReference nd))]              
                        (.setOffset nd (- xoffset (.getX nd)) 0.0)
                        (+ xoffset w spacing)))
                    0.0
                    (iterator-seq (.getChildrenIterator this)))
            (proxy-super layoutChildren)))]
    (doseq [n nodes]
      (add-child nd (as-node n)))
    nd))

;;like shelf, except it aligns children by
;;their midpoint of the vertical bounds as well.
;; (defn ^PNode ->mid-shelf
;;   [& nodes]
;;   (let [nd       
;;         (proxy [org.piccolo2d.PNode] []
;;           (layoutChildren []
;;             (reduce (fn [^double xoffset ^PNode nd]
;;                       (let [w (.getWidth (.getFullBoundsReference nd))]              
;;                         (.setOffset nd (- xoffset (.getX nd)) 0.0)
;;                         (+ xoffset w)))
;;                     0.0
;;                     (iterator-seq (.getChildrenIterator this)))
;;             (proxy-super layoutChildren)))]
;;     (doseq [n nodes]
;;       (add-child nd n))
;;     nd))


;;This is a bit off...
;;rewrite using our node transforms.
;;One option is to enforce layout constraints...
;;another option is to use translation nodes to
;;do our dirty work.
(defn ^PNode ->stack
  [& nodes]
  (let [nd 
        (proxy [org.piccolo2d.PNode] []
          (layoutChildren [] 
            (reduce (fn [^double yoffset ^PNode nd]
                      (let [bnds (.getFullBoundsReference nd)
                            h (.getHeight bnds )]              
                        (.setOffset nd 0.0 (+ yoffset h))
                        (+ yoffset h)))
                    0.0
                    (iterator-seq (.getChildrenIterator this)))
            (proxy-super layoutChildren)))]
    (doseq [n nodes]
      (add-child nd (as-node n)))
    nd))



(defn ^PNode ->spaced-stack
  [spacing & nodes]
  (let [nd 
        (proxy [org.piccolo2d.PNode] []
          (layoutChildren [] 
            (reduce (fn [^double yoffset ^PNode nd]
                      (let [bnds (.getFullBoundsReference nd)
                            h (.getHeight bnds )]              
                        (.setOffset nd 0.0 (+ yoffset h))
                        (+ yoffset spacing h)))
                    0.0
                    (iterator-seq (.getChildrenIterator this)))
            (proxy-super layoutChildren)))]
    (doseq [n nodes]
      (add-child nd (as-node n)))
    nd))


(defn ^PNode ->translate [x y child]
  (let [x  (double x)
        y  y ;(double (if *cartesian*  (- y) y))
        trans (doto (AffineTransform.)
                    (.translate (double x) (double y)))
        nd  (doto (PNode.) (.setTransform trans))]
     (add-child nd (as-node child))))

(defn ^PNode ->scale [xscale yscale child]
  (let [xscale  (double xscale)
        yscale  (double yscale)
        trans (doto (AffineTransform.)
                    (.scale (double xscale) (double yscale)))
        nd  (doto (PNode.)
              (.setTransform trans))
        ]
    (add-child nd (as-node child))))

(defn ->scaled-image [img xscale yscale & {:keys [id]}]
  (with-node-meta  
    (->scale xscale yscale
             (->image img))
    ((if id #(assoc % :id id) identity) 
     {:unscale [(/ 1.0 xscale) (/ 1.0 yscale)]      
      })))

(defn atom? [x] (instance? clojure.lang.Atom x))

;;Extra predicates for primitive node types.  Found these to
;;be somewhat useful.
(defn path-node? [nd] (instance? org.piccolo2d.nodes.PPath nd))
(defn text-node? [nd] (instance? org.piccolo2d.nodes.PText nd))
(defn image-node? [nd] (instance? org.piccolo2d.nodes.PImage nd))

;;I think this may be killing us.....
;;Supports animated, time-varying fade via the atom.
(defn ^PNode ->fade [alpha child]
  (let [alpha  (if (atom? alpha) alpha  (float alpha))
        push-tp (if (atom? alpha)
                  (fn [^org.piccolo2d.util.PPaintContext ppaint]                    
                    (.pushTransparency ppaint (float @alpha)))
                  (fn [^org.piccolo2d.util.PPaintContext ppaint]
                    (.pushTransparency ppaint ^float alpha)))
        pop-tp (if (atom? alpha)
                 (fn [^org.piccolo2d.util.PPaintContext ppaint]                   
                   (.popTransparency ppaint (float @alpha)))
                 (fn [^org.piccolo2d.util.PPaintContext ppaint]
                   (.popTransparency ppaint ^float alpha)))
        nd (if (atom? alpha)
             (proxy [org.piccolo2d.PNode] [] ;;possible trouble spot.
               (fullPaint [^org.piccolo2d.util.PPaintContext ppaint]
                 (when (>= @alpha 0)
                   (do (push-tp ppaint)
                       (proxy-super fullPaint ppaint)
                       (pop-tp ppaint)))))
             (proxy [org.piccolo2d.PNode] []
               (fullPaint [^org.piccolo2d.util.PPaintContext ppaint]
                 (do (push-tp ppaint)
                     (proxy-super fullPaint ppaint)
                     (pop-tp ppaint)))))
        _  (when (atom? alpha) ;;possible trouble spot.
             (add-watch alpha :fade (fn [k r old new]
                                      (when (not= old new)
                                       ; (do-scene ;;this didn't help us...
                                         (invalidate! ^PNode nd))))
        ;     )
             )]
    (add-child  nd (as-node child))))

;;We want to define a level-of-detail node...
;;specifically, nodes that change their detail/meaning
;;based on differing zooms.  This is like semantic zoom...
;;let's copy the example from the piccolo docs.
;;Ah....we just have the nodes drawn on top of
;;eachother....so another way to handle this is
;;via hiding/visibility....Certain nodes are only
;;visible at a the appropriate zoom....
;;Large and small should be the same size...
;;so the node bounds should be equivalent to large and small.
;;One trick is to just make the node disappear if it's out of scale...
;;note: scale is relative right?....
;;I guess we can just switch visibility....
(defn ^PNode ->semantic-node
  ([large  small thresh]
   (let [^PNode large (as-node large)
         ^PNode small (as-node small)
         state (atom :small)         
         toggle-state (fn toggle-state [newstate]
                        (do ;(println newstate)
                             (when (not= newstate @state)
                              (do 
                                  (reset! state newstate)
                                  (case newstate
                                    :large
                                    (do (.setVisible small false)
                                        (.setVisible large true))
                                    :small
                                    (do (.setVisible small true)
                                        (.setVisible large false)
                                        ))))
                            newstate))
         _ (toggle-state :large)
         ;;what about scale/size? 
         nd  (doto (proxy [org.piccolo2d.PNode] []
                     (fullPaint [^org.piccolo2d.util.PPaintContext ppaint]
                       (let [s (.getScale ppaint)]
                         (let [s (if (< s thresh)
                                   :small
                                   :large)
                               ]
                           (case (toggle-state s)
                             :small (.fullPaint small ppaint)
                             :large (.fullPaint large ppaint))))))
              (.setBounds (.getBounds small)))]
     (-> nd
         (add-child  large)
         (add-child small))))
  ([large small] (->semantic-node large small 1.0)))

(defn ->lod-box
  ([thresh icon node]
   (let [contents  (as-node node)
         bbox      (.getFullBounds contents)
         block     (cond  (keyword? icon)
                          (->filled-rect icon (.getX bbox) (.getY bbox) (.getWidth bbox) (.getHeight bbox))
                          (fn? icon)
                          (icon (.getX bbox) (.getY bbox) (.getWidth bbox) (.getHeight bbox))
                          :else
                          (as-node icon))]
     (->semantic-node contents block thresh)))
  ([thresh node] (->lod-box thresh :grey node)))
  

;;another idea is to use lod to only paint segments of children...
;;like....don't paint even children past a threshold....there are
;;lots of LOD strategies we can use...For instance, rather than
;;fading, paint solids...faster....
;; (defn ->lod-node [thresh node]
;;   (let [contents  (as-node node)
;;         bbox      (.getFullBounds contents)
;;         block     (->filled-rect :grey (.getX bbox) (.getY bbox) (.getWidth bbox) (.getHeight bbox))]
;;     (->semantic-node contents block thresh)))

(comment ;testing
  (defn random-coords [w h n]
    (repeatedly n (fn [] [(rand-int w) (rand-int h)])))
  (defn toggle-rect [big small x y w h]
    (if (identical? big small)
      (->filled-rect big x y w h)
      (->semantic-node (->filled-rect big x y w h)
                       (->filled-rect small x y w h))))
  (defn ->cloud
    ([n oncolor offcolor w h]
     (as-node (vec (for [[x y] (random-coords w h n)]
                     (toggle-rect oncolor offcolor x y 10 10)))))
    ([n color w h]  (->cloud n color color w h))     
    ([n w h] (->cloud n :green :blue w h))
    ([n] (->cloud n 600 600)))
  
  ;;nested-clouds....

  ;;so.....we can have an occluded container...
  ;;note:  I think primitives have a getBounds result....
  
  ;;each parent contains n children.
  ;;if level = 0, the children are regular clouds,
  ;;else,
  ;;  children are nested clouds....
  ;;  each child is scaled 0.01666 of the parent...
  ;;  so, at a given level, the child's scale is
  ;;  0.16 ^ level
  (defn random-color [] (java.awt.Color. (int (rand-int 255))
                                                 (int (rand-int 255))
                                                 (int (rand-int 255))))
  (defn nested-cloud [n level scale-factor init-scale x y w h]
    (if (zero? level)
      (->translate x y (->cloud (max (rand-int n) 1) 
                                (random-color)
                                (random-color)
                                w h))
      ;;translate and scale...      
       (let [current-scale  (* scale-factor init-scale)
             parent-color   (random-color)
            children (for [[x y] (random-coords w h (max (rand-int n) 1))]
                       (nested-cloud n
                                     (unchecked-dec level)
                                     scale-factor
                                     current-scale
                                     x y w h))]
      (->translate x y
         (->scale scale-factor scale-factor
                  (->lod-box  0.8 (fn [x y w h]
                                    (let [cx (+ x (/ w 2.0))
                                          cy (+ y (/ h 2.0))]
                                    (as-node [(->circle parent-color
                                                        cx
                                                        cy  w h)
                                              (->translate  cx  (+ cy (/ h 2.0))
                                                  (->scale 4.0 4.0 (->text  (str "Level : " level))))
                                              ])))
                              (as-node (vec children))))))))
    
  
  (defn sem-test []
    (let [boxes  (->cloud 1000)
          bbox   (.getFullBounds boxes)
          block  (->filled-rect :grey (.getX bbox) (.getY bbox) (.getWidth bbox) (.getHeight bbox))
          ]
      ;;we can create a node that toggles out at a zoom threshold to a grey box...
      (render! (->lod-box 0.25 boxes)   )))
  )

(defn ^PNode ->rotate [theta child]
  (let [theta (double theta)]                   
    (add-child  (doto (PNode.) (.rotate theta)) child)))


(defn dist [l r]
  (let [x1 (nth  l 0)
        y1 (nth  l 1)
        x2 (nth  r 0)
        y2 (nth  r 1)]
    [(- x2 x1)
     (- y2 y1)]))
(defn  norm [dx dy]
  (Math/sqrt (+ (Math/pow dx 2)
                (Math/pow dy 2))))

(defn direction
  [[x y]  [x2 y2]]
  (let [dx (-  x2 x)
        dy (- y2 y)
        norm (norm dx dy)]                                     
    [ (/ dx norm) (/ dy norm)]))

;;this is actually useful beyond just our node logic.
;;we'll pull it into the simulation stuff too.
;;Motion/animation...
;;Might be nice to have a general purpose path follower that
;;changes position on updates.  We have a point queue
;;that we can share, and the node will translate accordingly.
;;Then, we can just populate the point queue (like a channel)
;;with points from wherever.
(defn follow-path [x y speed  points]
  ;;translate a node, over time, going from point-to-point.
  (let [pos       (atom [x y]) ;arrays are mutable and fast.
        points    (drop-while (fn [p]
                                (let [[dx dx] (dist @pos p)]
                                   (and (zero? dx) (zero? dx))))
                          points)
        vel       (atom (let [[vx vy] (direction [x y] (first points))]
                          [(* vx speed)
                           (* vy speed)]))
        remaining (atom points)
        ]
    ;;this is really just a parametric curve.
    ;;we're walking as far as possible each time step.
    ;;governed by the constraints of the linear segments.
    ;;and speed/time.  Velocity and position are a consequence
    ;;of the walk.  deltas in position turn into node translations.
    (fn walk! [t]
      (when-let [pts (seq @remaining)]          
        (loop [available (* speed t)
               current   @pos
               pts      pts]          
            (cond (empty? pts)
                        (let [res (dist @pos current)
                              _    (reset! pos current)
                              _ (reset! remaining nil)]
                          ;(with-meta
                                        ;res
                          current
                          ;  {:point current}
                            ;)
                          )  ;last point, possible small step.
                  :else 
                  ;;we can keep walking       
                  (let [
                        target    (first pts)
                        [dx dy]   (dist current target)
                        required  (norm dx  dy)                  
                        covered   (-  available required)
                       ; _ (println [ current :-> target :-> [dx dy] :| available :/ required := covered])
                                   ]           
                    (if (pos? covered) ;we have excess travel capacity...
                      (do ;(println :passing-through target)
                          (recur covered
                                 target
                                 (rest pts)))
                  ;;we need to update our position
                  ;;we're in-transit how far along the segment?              
                  (let [;;scale that by unit velocity vector.
                        [vx vy] (direction current target) ;direction vector
                        dx    (* vx available) ;;displacement
                        dy    (* vy available)
                        destx (+ dx (first current)) ;actual end point.
                        desty (+ dy (second current))
                        ;;our displacement from original position
                       ; offx (- destx (nth @pos 0)) 
                       ; offy (- desty (nth @pos 1))
                       ; _ (println [:updating @pos :velocity [dx dy] :through current :to target ])
                        _    (reset! pos [destx
                                          desty])
                        _    (reset! remaining pts)]
                   ; (with-meta
                                        ;[offx offy]
                    [destx desty]
                      ;{:point @pos}
                    ;  )
                    ;;total displacement from current
                    ;{:position pos :velocity vel :remaining remaining}
                    )))))))))

;;follows along a path.
(defn follow-path!
  ([^PNode nd pts speed f]
   (let [bounds      (.getFullBounds nd)
         x           (.getX      bounds)
         y           (.getY      bounds)
         next-point (follow-path x y speed pts)]
     (fn [t]
       (when-let [^clojure.lang.PersistentVector res (next-point t)]        
         (f nd (double (.nth res 0)) (double (.nth res 1)))))))
  ([nd pts speed] (follow-path! nd pts speed translate!)))

(comment ;testing
  (def the-path (->orientedCurve :black 0 0 200 200))
  (def the-points (flatten-path the-path 1))
  (def the-glyph (->rect :red 0 0 10 10))
  (def update! (follow-path! the-glyph (cycle (concat the-points (reverse the-points))) 15))
  
)


;;simulates a node who's state changes over time.  Specifically, the
;;paint decays to nothing linearly.
;;returns a callback that can be used to decay the node.

(defn degrees [n] (* n (/ Math/PI 180.0)))

;;We really only apply the transformation once.
;;Any further translations that come through should be mirrored similarly.
;;Note:  It's not just a translation, it's also a scaling operation.
;;So, what we're doing is offseting the node by -y.
;;I think it's enough to just introduce a transform
(defn ^PNode ->cartesian
  ([height child]
   (binding [*cartesian* true] ;;this is going to cost us...
    (let [origin (-> (PNode.)                                      
                     (translate-by! 0.0 (- height))
                     (scale! 1.0 -1.0))]
      (add-child origin child))))
  ([^PNode child]
   (let [child (as-node child)]
     (->cartesian (.getHeight (.getFullBounds child)) child))))
          
;;general transform node.
(defn ^PNode ->transform [^java.awt.geom.AffineTransform xform child]
  (add-child (doto (PNode.) (.setTransform xform)) (as-node child)))

;;In this sense, the piccolo scenegraph is no different from
;;the existing hud (it's not being interacted with the user).
(defn ^PPath ->ellipse [^double x ^double y ^double w ^double h]    (PPath/createEllipse x y w  h))
;(defn ^PPath ->line    [x y x2  y2]   (PPath/createLine    x y x2 y2))
(defn ^PNode ->text    [^String txt]  (uncartesian!       (PText. txt)))

(defmacro ->button-strip [kvps]
  `(->shelf
    ~@(for [[k v] (seq kvps)]
        `(gui/button (str ~k)
                     (fn [~'_] ~v)))))

(def frame (atom nil))
(def canvas (atom nil))

(defn canvas? [x] (instance? org.piccolo2d.PCanvas x))
(defn show!
  ([cnv & {:keys [menu]}]
   (let [c (if  (canvas? cnv) cnv
                (doto (->canvas cnv)
                  (.setPreferredSize (java.awt.Dimension. 600 600))))
         _ (reset! canvas c)
         menuf (if menu #(gui/add-menu menu %) identity)
         f (->> (gui/display-simple  c)
                (menuf)
                (gui/toggle-top))]
     (reset! frame f)
     f)))

(defn get-canvas! []
  (when-let [f @frame]
    (.getComponent
     (.getContentPane f) 0)))
;;rotate all the rects....
(defn layer-bounds
  ([^PLayer lyr]  (.getGlobalFullBounds lyr)))

(defn center!
  [cnv]
  (doto (.. cnv getCamera)
    (.animateViewToCenterBounds  (layer-bounds (.getLayer cnv)) true 0)))

(defn with-input! [nd p]
  (do (.addInputEventListener  ^PNode (as-node nd)
                               ^PInputEventListener (events/as-listener p))
        nd))

(defn center-offscreen!
  ([cnv ^PBounds bnds]
   (doto (.. cnv getCamera)
     (.animateViewToCenterBounds  bnds true 0)))
  ([cnv] (center-offscreen! cnv (layer-bounds (.getLayer (.getCamera cnv) 0)))))


(defn offscreen! [nd & {:keys [transform background width height]
                        :or {width 600 height 600}}]
  (let  [cnv   (->offscreen-canvas width height)              
         layer (.getLayer (.getCamera cnv) 0)
         _     (when transform (.setTransform layer transform))
         _     (when background (if (or (node? background)
                                        (satisfies? IPiccNode background))
                                  (add-child! layer (as-node nd))
                                  (.setPaint (.getCamera cnv) background)))]
    (add-child cnv (as-node nd))
   ; (when handler (.addInputEventListener layer handler))
    (center-offscreen! cnv)
                                        ;(show! cnv)
    cnv
    ))

;;This allows us to render out to graphics.
(defn render-to [nd ^java.awt.Graphics2D graphics]
  (.render nd graphics))

;;I think the offscreeen canvas is just a picture....
;;Could be wrong though...
(extend-protocol spork.graphics2d.canvas/IShape
  POffscreenCanvas 
  (draw-shape [obj g]
    (do (.render obj g)
        g))
  (shape-bounds [obj]
    (let [^PBounds c (.getFullBounds (.getCamera obj))
          x (.getX c)          
          y (.getY c)
          width  (.getWidth  c)
          height (.getHeight c)]
      (spork.protocols.spatial/bbox x y  width height))))

(defn swing? [nd]
  (instance? org.piccolo2d.extras.pswing.PSwing nd))
(defn has-swing? [x]
  (some swing?  (node-seq (as-node x))))
                 
;;animate entities...
(defn render!  [nd & {:keys [transform background handler clear-pan? clear-zoom? swing? menu]}]
  (let  [cnv   (doto (if (or swing? (has-swing? nd))
                       (->swing-canvas)
                       (->canvas))
                   (.setPreferredSize (java.awt.Dimension. 600 600)))
           _     (when clear-pan?  (.removeInputEventListener cnv (.getPanEventHandler cnv)))
           _     (when clear-zoom? (.removeInputEventListener cnv (.getZoomEventHandler cnv)))
           layer (.getLayer cnv)
           _     (when transform  (.setTransform layer transform))
           _     (when background (if (or (node? background)
                                          (satisfies? IPiccNode background))
                                    (add-child! layer (as-node nd))
                                    (.setPaint (.getCamera cnv) background)))]
      (add-child! layer (as-node nd))
      (when handler (.addInputEventListener layer (events/as-listener handler)))
      (println (get-full-bounds layer))
      (show!   cnv :menu menu)
      (center! cnv)
      cnv))

(defn render-raw!  [nd & {:keys [transform background handler]}]
    (let  [cnv (doto (->canvas)
                     (.setPreferredSize (java.awt.Dimension. 600 600)))
           layer (.getLayer cnv)
           _     (when transform (.setTransform layer transform))
           _     (when background (if (or (node? background)
                                          (satisfies? IPiccNode background))
                                    (add-child! layer (as-node nd))
                                    (.setPaint (.getCamera cnv) background)))]
      (add-child! layer (as-node nd))
      (when handler (.addInputEventListener layer (events/as-listener handler)))
;      (center! cnv)
      (show! cnv)
      cnv))

(defn node->canvas  [nd & {:keys [transform background handler]}]
    (let  [cnv (doto (->canvas)
                     (.setPreferredSize (java.awt.Dimension. 600 600)))
           layer (.getLayer cnv)
           _     (when transform (.setTransform layer transform))
           _     (when background (if (or (node? background)
                                          (satisfies? IPiccNode background))
                                    (add-child! layer (as-node nd))
                                    (.setPaint (.getCamera cnv) background)))]
      (add-child! layer (as-node nd))
      (when handler (.addInputEventListener layer (events/as-listener handler)))
      (center! cnv)
      cnv))

(defn node->swingcanvas  [nd & {:keys [transform background handler]}]
    (let  [cnv (doto (->swing-canvas)
                     (.setPreferredSize (java.awt.Dimension. 600 600)))
           layer (.getLayer cnv)
           _     (when transform (.setTransform layer transform))
           _     (when background (if (or (node? background)
                                          (satisfies? IPiccNode background))
                                    (add-child! layer (as-node nd))
                                    (.setPaint (.getCamera cnv) background)))]
      (add-child! layer (as-node nd))
      (when handler (.addInputEventListener layer (events/as-listener handler)))
      (center! cnv)
      cnv))

(defn strokeable? [nd]  (instance? org.piccolo2d.nodes.PShape nd))
(defn highlight-stroke! [nd color]
    (let [^PNode nd     (as-node nd)
          old-stroke    (.getStroke nd)
          old-paint     (.getStrokePaint nd)
          new-stroke    (or old-stroke
                            (java.awt.BasicStroke.))
          ]
      (-> nd
          (vary-node-meta 
           assoc :highlighted [old-stroke old-paint])
          (stroke-paint! color)
          (stroke! new-stroke))))

(defn unhighlight-stroke! [nd]
   (if-let [hinfo (get (node-meta nd)
                        :highlighted)]
      (-> nd
          (vary-node-meta dissoc :highlighted)
          (stroke-paint! (second hinfo))
          (stroke! (first hinfo)))
      nd))

(defn ->bounds-rect [color nd]
  (let [^PNode nd (as-node nd)
        bounds    (.getFullBounds nd)]
    (->rect color 0.0 0.0
            (.getWidth  bounds)
            (.getHeight bounds))))

;;Problem is, if we add the child, we get the parent transforms
;;applied...
(defn highlight-bounds! [nd color]
  (let [hbounds (->bounds-rect color nd)]
      (-> nd
          (vary-node-meta 
           assoc :highlighted hbounds)
          (add-child hbounds))))

(defn unhighlight-bounds! [nd]
   (if-let [hinfo (get (node-meta nd)
                        :highlighted)]
      (-> nd
          (vary-node-meta dissoc :highlighted)
          (drop-child! hinfo)
          )
      nd))

(defn highlight! [nd color]
  (if (strokeable? nd)
    (highlight-stroke! nd color)
    (highlight-bounds! nd color)))

(defn un-highlight! [nd]
    (if-let [hinfo (get (node-meta nd)
                        :highlighted)]
      (if (strokeable? nd)        
        (unhighlight-stroke! nd)
        (unhighlight-bounds! nd))
      nd))

(defn highlighter [highlight-color]
  {:mouseEntered (fn [^PInputEvent e]
                   (highlight! (.getPickedNode e) highlight-color))                         
   :mouseExited  (fn [^PInputEvent e]
                   (un-highlight!  (.getPickedNode e)))})

(defn annotater [node->annotation]
  {:mouseEntered (fn [e] (node->annotation (events/picked-node e)))
   :mouseExited  (fn [^PInputEvent e]
                   (node->annotation  (.getPickedNode e)))})

;;We'd like to have an entity properties frame setup...
;;Should we have a jtree or just embed the piccolo canvas inside
;;a frame?

;;A property-box is just a scrollable grid of properties.
;;hmmm.....we need an input stream of information to display
;;Simplest is a text property.  If we include a simple function
;;to grab the node properties, then we can establish a mutable
;;map binding that we can observe.  Maybe wire up event listeners
;;to the node so that when the property changes, we notify the
;;property box.  Alternate idea is to use an existing control
;;like gridbox or something.
;;We could start with a simple idea...
;;Define a node that has an atom in its meta.
;;Put a watch on the atom that fires an event
;;on a listener on the node.  So, say we bind
;;text to the node...

;;For now, just use an atom...maybe use
;;channels later.  Looks a lot like a transducer.
;;So, this lets us bind stuff to the nodes.....
;;Alternate idea is to assoc a property listener with
;;the node.
(defn map-node [nd ref xform]
  (let [on-change (fn changed [k r old new]
                    (do-scene
                     (xform nd old new)))
        _          (add-watch ref :map-node  on-change)
        ]    
    nd
    ))

;;so

;;wheel-zooming...

;;wads controls (fps) for panning.
(defn center-on!
  ([^PCamera cam ^PNode nd ^long zoomtime]  
   (.animateViewToCenterBounds cam (.getFullBounds nd) true zoomtime))
  ([cam nd] (center-on! cam nd 0)))

(defn center-xform!
  ([^PCamera cam ^PNode nd ^long zoomtime]  
   (.animateViewToTransform cam (.getGlobalTransform nd) zoomtime))
  ([cam nd] (center-on! cam nd 0)))

(defn ^PAffineTransform as-xform [obj]
  (if (or (instance? org.piccolo2d.util.PAffineTransform obj)
          (instance? java.awt.geom.AffineTransform obj))
          obj
        (.getGlobalTransform ^PNode (as-node obj))))

(defn ^PAffineTransform inverse [inv]
  (.createInverse (as-xform inv)))

;;this is really similar to center-xform!
(defn animate-view-to-transform!
  ([cam x zoomtime]
   (.animateViewToTransform cam (as-xform x) zoomtime))
  ([cam x]
   (animate-view-to-transform! cam x 0)))

;;This kinda works...more useful for simple zooming...
(defn with-navigation [nd]
  (doto ^PNode (as-node nd)
        (.addInputEventListener (PNavigationEventHandler.))))

;;camera.animateViewToTransform(inverse, 500);

    ;; private PAffineTransform computeGlobalTransform(final PNode node) {
    ;;     if (node == null) {
    ;;         return new PAffineTransform();
    ;;     }

    ;;     final PAffineTransform parentGlobalTransform = computeGlobalTransform(node.parent);
    ;;     if (node.transform != null) {
    ;;         parentGlobalTransform.concatenate(node.transform);
    ;;     }
    ;;     return parentGlobalTransform;
;; }
;;this is private in piccolo.java, blah...
;;we'll write our own.  Ported straight from
;;piccolo2d's source.
(defn compute-global-transform [nd]
  (let [nd (and nd (as-node nd))]
    (if (not nd) (PAffineTransform.)
        (let [^PAffineTransform parentGlobalTransform
                (compute-global-transform (.getParent ^PNode nd))]
          (doto parentGlobalTransform
            (.concatenate (.getTransform ^PNode nd)))))))
                        
;;I think this is local->global?
(defn ^PAffineTransform global->local-transform
  ([nd ^PAffineTransform xform]
   (.getGlobalToLocalTransform
    ^PNode (as-node nd)
    xform
    ))
  ([nd] (global->local-transform nd (PAffineTransform.))))

(defn ^PAffineTransform local->global-transform
  ([nd ^PAffineTransform xform]
   (.getLocalToGlobalTransform
    ^PNode (as-node nd)
    xform
    ))
  ([nd] (local->global-transform nd (PAffineTransform.))))

(defn ^PAffineTransform global-transform [nd] (global->local-transform nd))
(defn ^PAffineTransform local-transform [nd] (local->global-transform nd))

;;can we visualize a marathon project?

(comment ;testing events
  (defn on-click! [nd f]  (with-input! nd {:mouseClicked  f}))
  (defn on-hover! [nd in out]
    (with-input! nd {:mouseEntered  in
                     :mouseExited   out}))
     

  
  (defn ->button [color x y w h label action]
    (let [state     (atom :up)          
          expanded  (as-node (->cartesian [(->filled-rect color x y w h)
                                           (->translate x y (->text label))]))]
      (on-click! expanded action)))
  
  (defn random-square [x y w h]
    (->filled-rect (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255))
                   x y w h))
  (defn random-squares [n w h]    
    (for [i (range n)]
      (random-square (* (rand) w) (* (rand) h) 50 50)))
  ;;note: this only works for node events.  If they're children,
  ;;the parent's never forward events to them.
  (defn ->hover-toggle [^PNode open ^PNode closed]
    (let [prior (atom nil)
          ep    (events/event-processor
                 :mouseEntered  (fn [e]
                                  (.setVisible open true)
                                  (.setVisible closed false)
                                  )
                 :mouseExited   (fn [e]
                                  (.setVisible open false)
                                  (.setVisible closed true)))
      _ (.setVisible closed false)]
      (doto (as-node [open closed])
        (.addInputEventListener ep))))
  
  (defn ->highlight-on-hover [color nd]
    (let [bnds (.getBounds nd)
          high (->rect color (.getX bnds) (.getY bnds) (.getWidth bnds) (.getHeight bnds))
          _    (.setVisible high false)
          ep   (events/event-processor
                 :mouseEntered  (fn [e]
                                  (.setVisible high true)
                                  )
                 :mouseExited   (fn [e]
                                   (.setVisible high false)
                                   ))]
      (doto (as-node [nd high])
        (.addInputEventListener ep))))
       
    (defn ->click-toggle [^PNode open ^PNode closed]
      (let [prior (atom true)
            ep    (events/event-processor
                   :mouseClicked  (fn [e]
                                    (swap! prior not)
                                    (.setVisible open @prior)
                                    (.setVisible closed (not @prior))
                                    ))
            _ (.setVisible closed false)]
        (doto (as-node [open closed])
          (.addInputEventListener ep))))
          
  (defn ->color-toggle [color nd]
    (let [prior (atom nil)
          ep (events/event-processor
              :mouseEntered  (fn [e]
                               (reset! prior (.getPaint nd))
                               (set-paint! nd color))
              :mouseExited   (fn [e]
                               (set-paint! nd @prior)))]
      (doto nd (.addInputEventListener ep))))

  (defn annotation [anno-node nd]
    (let [
          prior (atom nil)
          ep (events/event-processor
              :mouseEntered  (fn [e]
                               (reset! prior (.getPaint nd))
                               (set-paint! nd color))
              :mouseExited   (fn [e]
                               (set-paint! nd @prior)))]
      (doto nd (.addInputEventListener ep))))

  ;;so now we can interact...
  
;  (defn highlight! [border nd]
;    (let [bnds (.getBounds nd)
    
    (defn ->print-toggle [nd]
      (let [prior (atom nil)
            ep (events/event-processor
                :mouseEntered  (fn [e]                               
                                 (println "entered!")
                                 )
                :mouseExited   (fn [e]
                               (println "Exited!"
                                        ))
                :mouseMoved (fn [e] nil))]
        (doto nd (.addInputEventListener ep))))


    ;;Testing to correct layout problems with shelf and stack.
    
    )
    
            
(comment ;;testing menus
  (def simple-menu (gui/map->reactive-menu "Debug"
                                           {"Debug Item 1" "Hello"
                                            "Debug Item 2" "World"}))
  (def main-menu (gui/menu-bar (:view simple-menu)))
;;         debug-menu      (gui/map->reactive-menu "Debug"
;;                                                 debug-menu-spec)
;;         main-menu       (gui/menu-bar (:view project-menu)
;;                                       (:view processing-menu)
;;                                       (:view debug-menu))
  )
