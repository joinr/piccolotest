(ns piccolotest.sample
  (:require [spork.cljgui.components.swing :as gui]
            [spork.graphics2d.canvas :as canvas]
            [spork.graphics2d.swing :as swing])
  (:import
   [org.piccolo2d         PCanvas PLayer PNode PRoot]
   [org.piccolo2d.event   PBasicInputEventHandler PDragEventHandler
                          PInputEvent PInputEventFilter]
   [org.piccolo2d.nodes   PPath PImage PText]
   [org.piccolo2d.util   PBounds]
   [org.piccolo2d.extras.event
    PSelectionEventHandler
    PNotification
    PNotificationCenter]
   [java.awt Color Dimension Graphics2D]
   [java.awt.event   InputEvent MouseEvent]
   [java.awt.geom   Point2D]
   [javax.swing     JFrame]
   [java.util  ArrayList Random]))

;;This is a just a test space to work out some piccolo2d
;;apps.  Namely, to gain familiarity with the api,
;;and to figure out how we can wrap it in clojure easily.


;;THe basic workings of the piccolo2d api are frame(s), layers,
;;nodes, and a cameras.


;;the basis of everything is the frame.
;;Are there corresponding panels?
;;Can nodes be children of multiple layers?

;;PCanvas is a component, so we can use it like a jpanel.
(def my-canvas
  (doto 
      (PCanvas.)
    (.setPreferredSize (Dimension. 600 600))))

;;The canvas has a default or base layer.

;;Layer1 has a root, which is the default root associated with the PCanvas.
(def layer1 (.getLayer my-canvas))

;;this should be displayable as a jpanel.
;;we should be able to add a node.

;;One idea here is to use piccolo's event interface as a hookup
;;for view and rendering.  The nodes become part of the model.
;;As the simulation proceeds, we update the view by propogating
;;"movement" to the nodes. We basically simulate mouse movement
;;programmatically.

;;In this sense, the piccolo scenegraph is no different from
;;the existing hud (it's not being interacted with the user).
(defn ^PPath ->ellipse [x y w  h]  (PPath/createEllipse x y  w  h))
(defn ^PPath ->line    [x y x2 y2] (PPath/createLine    x y  x2 y2))
(defn ^PNode ->text    [^String txt] (PText. txt))

;;like the grapheditor example, we have a layer for nodes and a
;;layer for edges.

;;The layer idiom allows us to partition visible items.

;;In addition to the base layer, we can define other layers.
;;layer2 has no root.  It doesn't exist outside of the canvas
;;context.  We have to specifically add it...
(def layer2 (PLayer.))
(do (.addChild (.getRoot layer1) layer2))
;;Now, both layers have the same root node.

;;addChild is used to add nodes to layers (and other nodes).

;;There is only one root I think.  It's the root node for the scene graph,
;;and defines traversals.
;;(.addChild (.getRoot   this)   edge-layer)
;;Cameras have layers they are designated to "look at".

;;The canvas has a camera setup by default.
;;The camera cannot see it (and thus won't render it / traverse
;;its nodes) until we add it into the hierarchy.
(do (.addLayer (.getCamera my-canvas) 0 layer2))

;;let's hello-world it all together....
;;layer1 is for shapes
;;layer2 is for text...

;;This lets us maintain a database of layers->nodes.
;;A decent way to examine and query our scene.
;;We can start to define higher-order functions on
;;scenes (like mapping, filtering, and other traversals).
;;Can we derive scenes from existing scenes?
(def node-base (atom {}))

(defn add-shape! [nd]
  (.addChild layer1 nd))

(defn add-label! [nd]
  (.addChild layer2 nd))

(defn wipe-children! [nd])
(defn wipe-labels!   [])
(defn wipe-shapes!   [])

(defn ^PNode ->rect [color x y w h]
  (doto
      (PPath/createRectangle (double x) (double y) (double w) (double h))
      (.setPaint (swing/get-gui-color color))))

;;Note: the effect of layers rendered later is that
;;they overwrite earlier layers.


(comment
  
(add-label! (->text "Can you see me?"))


)
;;Also, the default is to set the node at the origin.



;;The most important thing about pathnodes is that they maintain a boundary (the path) of
;;where the node exists in space, and they supply bounds tests (via overrides of getBounds and
;;inBounds I think).

;;Lets make a bunch of infinite rectangles.

;;Can we define an event database?
;;How about event layers?
;;We already have samplers....
;;Can we define an eventdb that lets us see these samples over time?

(defn ^PNode ->random-rect []
  (doto (->rect (java.awt.Color. (int (rand-int 256)) (int (rand-int 256)) (int (rand-int 256))) 0 0 100 80)         
    (.translate (* 10000 (rand)) (* 10000 (rand)))))

(defn add-n-rects [& {:keys [n] :or {n 1000}}]
  (dotimes [i n]
    (add-shape! (->random-rect))))
    
;(extend-protocol clojure.lang.Seqable


(defn ^PNode rotate [^PNode n ^double deg]
  (doto n (.rotate deg)))

;; (defn rotate-rects! []
;;   (
(def frame (atom nil))

(defn show! []
  (let [f (gui/toggle-top (gui/display-simple my-canvas))]
    (reset! frame f)
    f))
;;rotate all the rects....

(defn layer-bounds [](.getGlobalFullBounds layer1))

(defn center! []
  (doto (.. my-canvas getCamera)
    (.animateViewToCenterBounds  (layer-bounds) true 0)))

;;we also want to do stuff...
(defn shapes [^PLayer l]
  (iterator-seq (.. l getChildrenReference iterator)))

(defn map-layer [f l]
  (transduce (map f) (fn [acc _] acc) l (shapes l)))
(defn do-layer [f l]
  (reduce (fn [acc s] (do (f s) acc)) l (shapes l)))

;;This is actually pretty performant, there's some garbage being generated
;;due to affine transforms.
(defn animate! []
  (future (while true (do (do-layer (fn [n] (rotate n (Math/toRadians 2.0))) layer1)
                          (.repaint ^PCanvas my-canvas)
                          (Thread/sleep 16)))))

;;We might consider wrapping an entire scene (a canvas), and providing idiomatic
;;bindings to alter it (either with callbacks or channels).

;;lets add some picking and see how the performance alters.
;;we can put a little gui hud together that displays the property list
;;for the selected object.  That should translate over to a track selector with
;;the hierarchy:
;;  tracks -> track -> event
;;For added utility, we'll add a timeline that keeps track with the
;;vertical time horizon of the current track.

;;a track is just a rectangle (as a background), with some events
;;laid out as children.
;;We then just filter based on event.
;;That's a way to add interactive layering (or alternately, add multiple layers).

(comment 
;; // Create a selection event handler
;; final PSelectionEventHandler selectionEventHandler = new PSelectionEventHandler(getCanvas().getLayer(),
;;         getCanvas().getLayer());
;; getCanvas().addInputEventListener(selectionEventHandler);
;; getCanvas().getRoot().getDefaultInputManager().setKeyboardFocus(selectionEventHandler);


;;No idea what this is doing, but we'll use it...


;; PNotificationCenter.defaultCenter().addListener(this, "selectionChanged",
;;         PSelectionEventHandler.SELECTION_CHANGED_NOTIFICATION, selectionEventHandler);

;;Just a guess, we eat two layers to define a selection input, not sure if this will interact poorly
;;with our pan handler.  This automatically installs a selection tool that will highlight
;;the  geometry, allow multiple selection semantics, etc.  
(def selected! ;;we can override the handling here...
  (PSelectionEventHandler.  layer1, layer1))

;;We can interact with the selection handler, like query the active selections.

;;Selection changes are friggin wierd.

;; (proxy [PSelectionEventHandler] [l r]
;;   (keyPressed [^PInputEvent ev]
;;     (do (proxy-super keyPressed ev)
;;         (println "keyPressed" ev))))

(def notifications (PNotificationCenter/defaultCenter))


(do (.addInputEventListener my-canvas selected!) ;break this out in a function
    (..  my-canvas getRoot getDefaultInputManager (setKeyboardFocus selected!))
    )
(comment
  
(defn update-edge
  "Draws this edge, either initially or after endpoint node has been moved."
  [node]
  (let [start (.. node getFullBoundsReference getCenter2D)]
    (.reset edge)
        (println (str "update-edge: draw from (" (.getX start) " "  (.getY start)
                      ") to (" (.getX end) " "  (.getY end) ")" ))
        (.moveTo edge (.getX start) (.getY start))
        (.lineTo edge (.getX end) (.getY end))))
)

(comment

(defn add-color-picker! []
  ;;Poached from greg's code.
  (let [filter (PInputEventFilter.)
        _      (.setOrMask filter (+ InputEvent/BUTTON1_MASK
                                     InputEvent/BUTTON3_MASK))
        custom-handler   ; its value is on next line
        
        (proxy [PDragEventHandler] []
          (mouseEntered [^PInputEvent e]
            (proxy-super mouseEntered e)
            (if (= (.getButton e) MouseEvent/NOBUTTON)
              (. (. e getPickedNode) setPaint Color/RED)))
          (mouseExited [^PInputEvent e]
            (proxy-super mouseExited e)
            (if (= (.getButton e) MouseEvent/NOBUTTON)
              (. (. e getPickedNode) setPaint Color/WHITE)))
          (startDrag [^PInputEvent e]
            (proxy-super startDrag e)
            (.setHandled e true)
            (. (. e getPickedNode) moveToFront))
          (drag [^PInputEvent e]
            (proxy-super drag e)
            (let [node (.getPickedNode e)
                  edges (.getAttribute node "edges")
                  num-used-limit (.getAttribute node "num-used")]
              (loop [index 0]
                (if (< index num-used-limit)
                  (do
                    (update-edge (aget edges index))
                    (recur (inc index))))))))]
    (doto custom-handler (.setEventFilter  filter))
    (.addInputEventListener l custom-handler)))
)
