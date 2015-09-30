(ns piccolotest.sample
  (:require [spork.cljgui.components.swing :as gui]
            [spork.graphics2d.canvas :as canvas]
            [spork.graphics2d.swing :as swing]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]])
  (:import
   [org.piccolo2d         PCanvas PLayer PNode PRoot]
   [org.piccolo2d.event   PBasicInputEventHandler PDragEventHandler
                          PInputEvent PInputEventFilter]
   [org.piccolo2d.nodes   PPath PImage PText PShape]
   [org.piccolo2d.util   PBounds]
   [org.piccolo2d.extras.event
    PSelectionEventHandler
    PNotification
    PNotificationCenter]
   [org.piccolo2d.extras.pswing PSwing PSwingCanvas ]
   [org.piccolo2d.extras.swing SwingLayoutNode]
   [org.piccolo2d.extras.swing.SwingLayoutNode.Anchor]

   [java.awt Color Dimension Graphics2D  GridBagConstraints GridBagLayout BorderLayout FlowLayout 
    GridLayout  Component Insets]
   [java.awt.event   InputEvent MouseEvent]
   [java.awt.geom   Point2D]
   [javax.swing     JFrame JPanel Box BoxLayout] 
   [java.util  ArrayList Random]))

;;Useful protocol 

;;nodes communicate via a node channel
(def node-channel (chan (a/dropping-buffer 100)))

(defn notify! [msg]
  (go (a/put! node-channel msg)))

(defn notify!! [msg]
  (>!! node-channel msg))

(defprotocol MetaNode
  (node-meta [nd])
  (with-node-meta [nd m]))
                       
(extend-type org.piccolo2d.PNode
  MetaNode
  (node-meta [obj]         (.getAttribute obj "meta"))
  (with-node-meta [obj m]  (.addAttribute obj "meta" m) obj))

(defn ^PNode ->rect
  ([color x y w h meta]
   (->
     (doto
       (PPath/createRectangle (double x) (double y) (double w) (double h))
       (.setPaint (swing/get-gui-color color)))
     (with-node-meta meta)))
  ([color x y w h] (->rect color x y w h {})))


(defn ^PNode ->image
  ([source meta] (->  (PImage. source)
                      (with-node-meta meta)))
  ([source] (->image source {})))


;(defn beside [xs])
;(defn above  [xs])
    
(defn ->cell [row col w h]
  (->rect :white (* col w)
          (* row h) w h {:row row :col col}))

(defn add-child! [^PNode p ^PNode c] (doto p (.addChild c)))

;;so layers can act like groups.  They are also nodes...
(defn ->layer
  ([xs meta]
   (reduce (fn [^PLayer acc ^PNode n]
             (doto acc (.addChild n)))
           (doto (PLayer.) (.addAttribute "meta" meta)) xs))
  ([xs] (->layer xs   {}))
  ([]   (->layer nil  {})))


(defn ->panel [^JPanel pnl]
  (doto (PSwing. pnl)
    ;(.setUseBufferedPainting true)
    ))

;;note: a canvas is a panel...
(defn ->swing-canvas [pnl]
  (doto 
      (PSwingCanvas.)
    (.addChild pnl)))


;;This works just like our good old fashioned combinators from cljgui
(defn ^PNode shelf [& components]
  (let [shelf (SwingLayoutNode. ) ]
    (doseq [c components] (.addChild shelf c))
    shelf))

(defn ^PNode stack [& components]
  (let [container (JPanel.)
        box (BoxLayout. container BoxLayout/PAGE_AXIS)
        _ (.setLayout container box)
        stack (SwingLayoutNode. container)]
     (doseq [^PNode c components]
       ;(.setAlignmentX c Component/CENTER_ALIGNMENT)
       (.addChild stack 
            c))
    stack))


;;we'd like to add listeners...

;;a table is a layer of cells.
;;canvas is a panel, layers are groupings of shapes that confer
;;and respond to events with eachother.
(defn ->table [rows cols w h]
  (let [cell-data   (into []
                      (for [row (range rows)
                            col (range cols)]
                       [[row col] (->cell row col w h)]))
        cells  (reduce (fn [acc [[row coll] c]]
                         (let [rw (get acc row {})
                               cols (assoc rw coll c)]
                           (assoc acc row cols))) {} cell-data)]        

       ; background (->rect :white 0 0 (* w cols) (* h rows) {:cells cells})]
       (->layer (map second  cell-data) {:cells cells})))

(defn ->srm-table [units w h] (->table units 28 60 20))
(defn cells [t] (:cells (node-meta t)))
(defn get-cell [t row col] (get (get (cells t) row) col))
;;we'd like to listen to individual cells too...


;;We can create a node canvas....
;;Basically, we want to maintain information on every node...
;;We do that via meta (attributes)
;;If we have more than one logical layer, we have different
;;picking criteria....

;;creates a collection of "chunks" that correspond to the table
;;we see.  The difference here is that we maintain a database
;;of abstract layers, which allows us to filter shapes
;;programatically...We'll determine layers totally
;;by querying, so we have a declarative visual querying language.
;;That way, we can dynamically filter nodes by attributes
;;like time and state.
;;we're doing this by composing multiple nodes, but it may be
;;possible to build our own singleton node.




;; (defn ->cells [n qtrs]
 
;;   )


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


;;We can store all the info in the metadata of the nodes...                     
(def myfilter (doto (PInputEventFilter.)
                    (.setOrMask (+ InputEvent/BUTTON1_MASK
                                   InputEvent/BUTTON3_MASK))))                     

;;let's have the handler communicate via channels....                     
;;we can add this to the layer.
(def my-handler 
  (let [old-paint (atom nil)]
    (doto 
        (proxy [PDragEventHandler] []
          (mouseEntered [^PInputEvent e]
            (proxy-super mouseEntered e)
            (when (= (.getButton e) MouseEvent/NOBUTTON)
              (let [^PNode nd (.getPickedNode e)
                    _ (reset! old-paint (.getPaint nd))]
                (.setPaint nd Color/RED))))
          (mouseExited [^PInputEvent e]
            (proxy-super mouseExited e)
            (when (= (.getButton e) MouseEvent/NOBUTTON)
              (let [^PNode nd (.getPickedNode e)
                    clr @old-paint] 
                (.setPaint nd @old-paint))))
          (startDrag [^PInputEvent e]
            (proxy-super startDrag e)
            (.setHandled e true)
            (. (. e getPickedNode) moveToFront))
          (drag [^PInputEvent e]
            (proxy-super drag e)
        )
          )
      (.setEventFilter myfilter)
    )))

(do (.addInputEventListener layer1 my-handler))


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
(defn wipe-shapes!   [] (.removeAllChildren layer1))



;;Note: the effect of layers rendered later is that
;;they overwrite earlier layers.


(comment
  
(add-label! (->text "Can you see me?"))


)

;(defn ->labeled-box [lbl 
;;Also, the default is to set the node at the origin.



;;The most important thing about pathnodes is that they maintain a boundary (the path) of
;;where the node exists in space, and they supply bounds tests (via overrides of getBounds and
;;inBounds I think).

;;Lets make a bunch of infinite rectangles.

;;Can we define an event database?
;;How about event layers?
;;We already have samplers....
;;Can we define an eventdb that lets us see these samples over time?
(def rcount (atom 0))

(defn merge-att [^PShape n m]
  (doseq [[k v] m]
    (.addAttribute n (str k) (str v)))
  n)


                     
(defn ^PNode ->random-rect [& {:keys [meta]}]
  (let [n @rcount
        _ (swap! rcount inc)]
    (-> (doto (->rect (java.awt.Color. (int (rand-int 256)) (int (rand-int 256)) (int (rand-int 256))) 0 0 100 80)         
          (.translate (* 10000 (rand)) (* 10000 (rand)))
          (merge-att {"name" (str "Rect_" n)})))))

(defn add-n-rects [& {:keys [n] :or {n 1000}}]
  (dotimes [i n]
    (add-shape! (->random-rect))))
    

(defn ^PNode rotate [^PNode n ^double deg]
  (doto n (.rotate deg)))

;; (defn rotate-rects! []
;;   (
(def frame (atom nil))

(defn show! []
  (let [f (gui/toggle-top ;(gui/shelf
           (gui/display-simple my-canvas))
    ;    )
    ]
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


(defn ->shelf
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
      (.addChild nd n))
    nd))

(defn ->stack
  [& nodes]
  (let [nd 
        (proxy [org.piccolo2d.PNode] []
          (layoutChildren [] 
            (reduce (fn [^double yoffset ^PNode nd]
                      (let [h (.getHeight (.getFullBoundsReference nd))]              
                        (.setOffset nd 0.0 (- yoffset (.getX nd)))
                        (+ yoffset h)))
                    0.0
                    (iterator-seq (.getChildrenIterator this)))
            (proxy-super layoutChildren)))]
    (doseq [n nodes]
      (.addChild nd n))
    nd))


(comment
  (require 'quilsample.core)
  (defn pswing-example []
    (let [sw (PSwingCanvas.)
          lbl (gui/label "Day: ")
          _   (add-watch quilsample.core/global-time :ticker
                        (fn [k r old new]
                         (.setText lbl (str "Day: " new))))
          _ (doto sw (.setPreferredSize (Dimension. 800 600)))
          rootlayer  (.getLayer sw)
          
          [board dwells states fills chunks lbl :as panels] (mapv ->panel [(quilsample.core/board)
                                                                       (quilsample.core/dwells)
                                                                       (quilsample.core/state-trend-widget)
                                                                       (quilsample.core/fills)
                                                                       (quilsample.core/chunks)
                                                                       lbl])]
      (do (.addChild rootlayer (->stack
                                (->shelf
                                 (->stack
                                  fills
                                  board
                                  )
                                 (->stack states dwells)                                                                         
                                 chunks
                                 lbl)
          )))
       (gui/toggle-top (gui/display-simple sw))))

;; (defn hud-panel []
;;   (let [lbl (swing/label "Day: ")
;;         _   (add-watch global-time :ticker
;;                        (fn [k r old new]
;;                          (.setText lbl (str "Day: " new))))
;;         ]
;;     (swing/stack
;;      (swing/shelf
;;       (swing/stack
;;        (swing/shelf (dwells  :height 350 :width 350)
;;                     (state-trend-widget :width 350 :height 350))       
;;        (swing/shelf (board :width 300 :height 300)
;;                     (swing/label "          ")
;;                     (board :width 300 :height 300)                                 
;;                     ))
;;       (chunks))
;;      lbl)))

  ;;implementing our hud in piccolo....trying to squeak out some usability

  ;;The best thing here is to implement everything in piccolo from scratch...
  ;;We "could" implement a combinator library that draws our shapes in a reified scene...
  ;;We basically build a new piccolo node every time (not unlike what we're doing now),
  ;;and create a scene graph ad-hoc.
  ;;So, our combinators change a little.  Now, we just return a graph of nodes
  ;;that alter their children....Ah....now I get it.
  ;;The only difference between this and the ishape methods I use is the existence of
  ;;graphical bounds and event listeners on each node (built into the PNode superclass).
  (defn p-hud []
    (let [init! (fn [pp] ((:painter (.state pp)) (:bg (.state pp))))
          cnv      (doto (PCanvas.) (.setPreferredSize (Dimension. 600 600)))
          rootnode (.getLayer cnv)
          ds       (quilsample.core/dwells :height 350 :width 350)
          states   (quilsample.core/state-trend-widget :width 350 :height 350)
          brd      (quilsample.core/board :width 300 :height 300)
          chnks    (quilsample.core/chunks)
          bimage   (->image (:buffer @brd ))
          bimage2  (->image (:buffer @brd ))
          dsimage  (->image (:buffer @ds))
          simage   (->image (:buffer @states))
          cimage   (->image (:buffer @chnks))
          _ (doseq [c [ds states brd chnks]] (init! c))
         ; _ (gui/view (:buffer @b))
          lbl (gui/label "Day: ")
         _   (add-watch quilsample.core/global-time :ticker
                        (fn [k r old new]
                          (do  (.setText lbl (str "Day: " new))
                               (.repaint cnv))))
          portrait          ; (shelf
                            ;  (stack
                               (shelf dsimage
                                      simage)       
                             ;  (shelf bimage                    
                              ;        bimage2))
                              
                             ; cimage)
          _ (.addChild rootnode portrait)
          ]          
      ;(gui/stack
        cnv
        ;lbl
        ))
  ;)
    )
;;note: we can keep our nodes in a persistent structure, and still have the
;;ability to map over them.  They'll mutate their internal state as necessary, but                     
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

;;sets us up to only pan and zoom when ctrl is down.  We can probably
;;shift this to be opposite, so we only have selection when ctrl is down.                     
(defn alter-defaults []
  (let [ph (.getPanEventHandler my-canvas)
        zh (.getZoomEventHandler my-canvas)
        pf (.getEventFilter ph)
        zf (.getEventFilter zh)
 ;       pm (.setOrMask pf)
        _  (.setAndMask pf (+ InputEvent/BUTTON1_MASK  InputEvent/CTRL_MASK))
;        zm (.setOrMask zf)
        _  (.setAndMask zf ( + InputEvent/BUTTON3_MASK InputEvent/CTRL_MASK))
        ]
    nil))

;;another option is to define a custom listener, and attach it to every node...
;;so that when the node is picked, dragged, etc., we get a hook onto the activity.

;;The selection activity is a little too much atm, let's stay simpler.                     

                     
(comment 
;; // Create a selection event handler
;; final PSelectionEventHandler selectionEventHandler = new PSelectionEventHandler(getCanvas().getLayer(),
;;         getCanvas().getLayer());
;; getCanvas().addInputEventListener(selectionEventHandler);
;; getCanvas().getRoot().getDefaultInputManager().setKeyboardFocus(selectionEventHandler);


;;No idea what this is doing, but we'll use it...

  (defn remove-defaults []
    (do (.removeInputEventListener  my-canvas (.getPanEventHandler my-canvas))
        (.removeInputEventListener  my-canvas (.getZoomEventHandler my-canvas))))

;; PNotificationCenter.defaultCenter().addListener(this, "selectionChanged",
;;         PSelectionEventHandler.SELECTION_CHANGED_NOTIFICATION, selectionEventHandler);

 ;; (proxy [PDragEventHandler] []
 ;;          (mouseEntered [^PInputEvent e]
 ;;            (proxy-super mouseEntered e)
 ;;            (if (= (.getButton e) MouseEvent/NOBUTTON)
 ;;              (. (. e getPickedNode) setPaint Color/RED)))
 ;;          (mouseExited [^PInputEvent e]
 ;;            (proxy-super mouseExited e)
 ;;            (if (= (.getButton e) MouseEvent/NOBUTTON)
 ;;              (. (. e getPickedNode) setPaint Color/WHITE)))
 ;;          (startDrag [^PInputEvent e]
 ;;            (proxy-super startDrag e)
 ;;            (.setHandled e true)
 ;;            (. (. e getPickedNode) moveToFront))
 ;;          (drag [^PInputEvent e]
 ;;            (proxy-super drag e)
 ;;            (let [node           (.getPickedNode e)
 ;;                  edges          (.getAttribute node "edges")
 ;;                  num-used-limit (.getAttribute node "num-used")]
 ;;              (loop [index 0]
 ;;                (if (< index num-used-limit)
 ;;                  (do
 ;;                    (update-edge (aget edges index))
 ;;                    (recur (inc index))))))))

  (defn register! [ name f source]
    (let [nc  (.. PNotificationCenter defaultCenter)]
      (.addListener nc f "invoke" (str name) source)
      nil))
;;Just a guess, we eat two layers to define a selection input, not sure if this will interact poorly
;;with our pan handler.  This automatically installs a selection tool that will highlight
;;the  geometry, allow multiple selection semantics, etc.  
(def selected! ;;we can override the handling here...
  (let [sel  (PSelectionEventHandler.  layer1, layer1)
        ef   (.getEventFilter sel)
        _    (.setAndMask ef  (+ InputEvent/BUTTON1_MASK  InputEvent/SHIFT_MASK))]
    sel))

;;We can interact with the selection handler, like query the active selections.

;;Selection changes are friggin wierd.

;; (proxy [PSelectionEventHandler] [l r]
;;   (keyPressed [^PInputEvent ev]
;;     (do (proxy-super keyPressed ev)
;;         (println "keyPressed" ev))))


;;ideally, we toggle the selected nodes...
(def notifications (PNotificationCenter/defaultCenter))
;;Ths attached our canvas selector....we should have it filter so that we
;;only every select if the ctrl key is down.
(do (.addInputEventListener my-canvas selected!) ;break this out in a function
    (..  my-canvas getRoot getDefaultInputManager (setKeyboardFocus selected!))
    )

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
            (let [node           (.getPickedNode e)
                  edges          (.getAttribute node "edges")
                  num-used-limit (.getAttribute node "num-used")]
              (loop [index 0]
                (if (< index num-used-limit)
                  (do
                    (update-edge (aget edges index))
                    (recur (inc index))))))))]
    (doto custom-handler (.setEventFilter  filter))
    (.addInputEventListener l custom-handler)))
)


                     
        ;; camera.addInputEventListener(new PBasicInputEventHandler() {
        ;;     public void mouseMoved(final PInputEvent event) {
        ;;         updateToolTip(event);
        ;;     }

        ;;     public void mouseDragged(final PInputEvent event) {
        ;;         updateToolTip(event);
        ;;     }

        ;;     public void updateToolTip(final PInputEvent event) {
        ;;         final PNode n = event.getPickedNode();
        ;;         final String tooltipString = (String) n.getAttribute("tooltip");
        ;;         final Point2D p = event.getCanvasPosition();

        ;;         event.getPath().canvasToLocal(p, camera);

        ;;         tooltipNode.setText(tooltipString);
        ;;         tooltipNode.setOffset(p.getX() + 8, p.getY() - 8);
        ;;     }
        ;; }
