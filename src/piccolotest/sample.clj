(ns piccolotest.sample
  (:require [spork.cljgui.components.swing :as gui]
            [spork.graphics2d.canvas :as canvas]
            [spork.geometry.shapes :as shapes]
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
   [org.piccolo2d.extras.nodes PNodeCache]
   [java.awt Color Dimension Graphics2D  GridBagConstraints GridBagLayout BorderLayout FlowLayout 
    GridLayout  Component Insets]
   [java.awt.event   InputEvent MouseEvent]
   [java.awt.geom   Point2D AffineTransform]
   [javax.swing     JFrame JPanel Box BoxLayout] 
   [java.util  ArrayList Random]))

;;Useful protocol 

;;nodes communicate via a node channel
(def node-channel (chan (a/dropping-buffer 100)))
(def ^:dynamic *cartesian* true)

(defn notify! [msg]
  (go (a/put! node-channel msg)))

(defn notify!! [msg]
  (>!! node-channel msg))

(defn add-child! [^PNode p ^PNode c] (doto p (.addChild c)))
(defn drop-child! [^PNode p ^PNode c] (doto p (.removeChild c)))

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

;;We also w

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
  (as-node   [nd] nd)
  (add-child [nd chld] (do (.addChild nd (as-node chld)) nd))
  org.piccolo2d.PCanvas
  (as-node   [nd]        (.getRoot ^PCanvas nd))
  (add-child [nd chld] (do (.addChild (.getLayer nd) (as-node chld)) nd))
  org.piccolo2d.extras.pswing.PSwingCanvas
  (as-node [nd]        (.getRootNode ^PSwingCanvas nd))
  (add-child [nd chld] (do (.addChild (.getLayer ^PSwingCanvas nd) (as-node chld)) nd))
  clojure.lang.PersistentVector
  (as-node [nd]        (reduce add-child (PNode.)  nd))
  (add-child [nd chld] (conj nd))
  javax.swing.JPanel
  (as-node [nd]   (PSwing. nd))
  (add-child [nd child] (add-child (PSwing. nd) child))
  javax.swing.JComponent
  (as-node [nd]   (PSwing. nd))
  (add-child [nd child] (add-child (PSwing. nd) child)))
                      

(defn ->cache [child]
  (let [c (PNodeCache.)
        ]
    (add-child c child)))

(defn ^PNode set-paint! [^PNode nd clr]
  (doto nd
    (.setPaint ^java.awt.Color (swing/get-gui-color clr))))
(defn ^PNode invalidate! [^PNode nd]
  (doto nd
    (.invalidatePaint)))

(comment 
(dotimes [i 1000]
  (let [[c r] [(rand-int 28) (rand-int 60)]]
    (-> (get-cell tbl r c)
        (set-paint! (rand-nth [:red :blue :orange :yellow :green]) )
        (invalidate!))
    (Thread/sleep 16)))
)
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

(defn ->canvas
  ([]  (PCanvas.))
  ([& nodes] (reduce add-child (PCanvas.) nodes)))

;(defn ->layer  []  (PLayer.))

;;we should redefine this relative to cartesian coords.
(defn ^PNode translate!
  [^PNode nd ^double x ^double y]
   (doto nd (.translate x y)))

;;probably make this something else...
(defn ^PNode translate
  [^PNode nd ^double x ^double y]
   (doto nd (.translate x (- y))))

(defn ^PNode translate-to! [^PNode nd ^double x ^double y]
  (.setGlobalTranslation nd (java.awt.geom.Point2D$Double. x y)))

(defn translate-by! [^PNode nd ^double x ^double y ]
  (let [trans (doto (AffineTransform.)
                    (.translate (double x) (double (if *cartesian* (- y) y))))]
    (doto nd (.setTransform trans))))

(defn ^PNode scale! [^PNode nd  xscale yscale]
  (let [xscale  (double xscale)
        yscale  (double yscale)
        trans (doto (AffineTransform.)
                (.scale (double xscale) (double yscale)))]
    (doto nd (.transformBy trans))))

;;I think we want this to be 0.0 for the x coordinate, not 1.0....
(defn ^PNode uncartesian! [^PNode nd]
  (let [height (.getHeight nd)]
    (-> nd 
        (translate! 0.0 height) ;altered from 1.0
        (scale! 1.0 -1.0))))
;(defn ^PNode scale! [^PNode nd ^double x ^double y] (doto nd (.scale x y)))

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
(defn ->sketch
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
      (add-child nd n))
    nd))

;;rewrite using our node transforms.
(defn ->stack
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
      (add-child nd n))
    nd))

(defn ->translate [x y child]
  (let [x  (double x)
        y  y ;(double (if *cartesian*  (- y) y))
        trans (doto (AffineTransform.)
                    (.translate (double x) (double y)))
        nd  (doto (PNode.) (.setTransform trans))]
     (add-child nd child)))

;(defn ->ctrans [x y nd] (->translate x ( - y) nd))

(defn ->scale [xscale yscale child]
  (let [xscale  (double xscale)
        yscale  (double yscale)
        trans (doto (AffineTransform.)
                    (.scale (double xscale) (double yscale)))
        nd  (doto (PNode.)
              (.setTransform trans))
        ;; nd (proxy [org.piccolo2d.PNode] []
        ;;      (layoutChildren [] 
        ;;        (reduce
        ;;         (fn [acc ^PNode nd]           
        ;;           (.scale nd xscale yscale))
        ;;         nil
        ;;         (iterator-seq (.getChildrenIterator this)))
        ;;        (proxy-super layoutChildren)))
        ]
    (add-child nd child)))

(defn ->fade [alpha child]
  (let [alpha (float alpha)        
        nd (proxy [org.piccolo2d.PNode] []
             (fullPaint [^org.piccolo2d.util.PPaintContext ppaint]
               (do (.pushTransparency ppaint alpha)
                   (proxy-super fullPaint ppaint)
                   (.popTransparency ppaint alpha))))]                   
    (add-child  nd child)))

(defn ->rotate [theta child]
  (let [theta (double theta)]                   
    (add-child  (doto (PNode.) (.rotate theta)) child)))

(defn degrees [n] (* n (/ Math/PI 180.0)))

;;We really only apply the transformation once.
;;Any further translations that come through should be mirrored similarly.
;;Note:  It's not just a translation, it's also a scaling operation.
;;So, what we're doing is offseting the node by -y.
;;I think it's enough to just introduce a transform
(defn ->cartesian [height child]
  (binding [*cartesian* true]
    ;; (let [nds (if (seq child) (vec child) [child])
    ;;       nd (proxy [org.piccolo2d.PNode] []
    ;;            (layoutChildren [] 
    ;;              (reduce
    ;;               (fn [acc ^PNode nd]
    ;;                 (let [bnds (.getFullBounds nd)]
    ;;                   (translate! nd 0.0 ;;this is concatenating.  We're doing a matrix multiply every time.
    ;;                                  (- height (.getHeight bnds)))))
    ;;               nil
    ;;               (iterator-seq (.getChildrenIterator this)))
    ;;              (proxy-super layoutChildren)))]
    (let [origin (-> (PNode.)                                      
                   (translate-by! 0.0 (- height))
                   (scale! 1.0 -1.0))]
      (add-child origin child))))

          
;;general transform node.
(defn ->transform [^java.awt.geom.AffineTransform xform child]
  (add-child (doto (PNode.) (.setTransform xform)) child))
    
(defn ->cell [row col w h]
  (->rect :white (* col w)
          (* row h) w h {:row row :col col}))



;; ;;This works just like our good old fashioned combinators from cljgui
;; (defn ^PNode shelf [& components]
;;   (let [shelf (SwingLayoutNode. ) ]
;;     (doseq [c components] (.addChild shelf c))
;;     shelf))

;; (defn ^PNode stack [& components]
;;   (let [container (JPanel.)
;;         box (BoxLayout. container BoxLayout/PAGE_AXIS)
;;         _ (.setLayout container box)
;;         stack (SwingLayoutNode. container)]
;;      (doseq [^PNode c components]
;;        ;(.setAlignmentX c Component/CENTER_ALIGNMENT)
;;        (.addChild stack 
;;             c))
;;      stack))

;;we'd like to add listeners...


;;It'd be nice to have a value set in the table, as well as a mapping of
;;value->color.  We can have multiple layers too, allowing for imagery.

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
    ;(->cache
     (->layer  (map second  cell-data) {:cells cells})))
  ;)

;(defn dumb-listener 

(defn ->srm-table [units qtrs] (->table units qtrs 60 20))
(defn cells [t] (:cells (node-meta t)))
(defn get-cell [t row col] (get (get (cells t) row) col))
(defn clear-cells! [t]
  (reduce-kv (fn [acc _ xs]
               (reduce-kv 
                (fn [acc _ ^PNode nd]
                  (do (.removeAllChildren nd)
                      (set-paint! nd :white)
                      acc))
                acc
                xs))
             t
             (cells t)))

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
(defn ^PPath ->ellipse [x y w   h]    (PPath/createEllipse x y w  h))
(defn ^PPath ->line    [x y x2  y2]   (PPath/createLine    x y x2 y2))
(defn ^PNode ->text    [^String txt]  (uncartesian!       (PText. txt)))

         
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

(defn ^org.piccolo2d.event.PInputEventListener ->simple-handler [f]
  (proxy [org.piccolo2d.event.PInputEventListener] []
    (processEvent [^org.piccolo2d.event.PInputEvent e  type]
      (let [^int t type]
        (f e)))))

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
            (. (. e getPickedNode) raiseToTop))
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
(def rcount
  (atom 0))

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

(def frame (atom nil))

(defn canvas? [x] (instance? org.piccolo2d.PCanvas x))

(defn show!
  ([cnv]
   (let [f (gui/toggle-top
            (gui/display-simple
            ;org.piccolo2d.extras.PFrame. "Canvas" false
             (if  (canvas? cnv) cnv
                  (doto (->canvas cnv)
                    (.setPreferredSize (java.awt.Dimension. 600 600))))))]
     (reset! frame f)
     f))
  ([] (show! my-canvas)))

;;rotate all the rects....
(defn layer-bounds
  ([^PLayer lyr]  (.getGlobalFullBounds lyr)))

(defn center!
  ([cnv]
   (doto (.. cnv getCamera)
     (.animateViewToCenterBounds  (layer-bounds (.getLayer cnv)) true 0)))
  ([] (center! my-canvas)))

;;we also want to do stuff...
(defn shapes [^PLayer l]
  (iterator-seq (.. l getChildrenReference iterator)))

(defn map-layer [f l]
  (transduce (map f) (fn [acc _] acc) l (shapes l)))
(defn do-layer [f l]
  (reduce (fn [acc s] (do (f s) acc)) l (shapes l)))

;; (defn ->property-listener [pmap]
;;   (proxy [


(comment
  (require 'quilsample.core)
  (defn pswing-example []
    (let [sw  (PSwingCanvas.)
          lbl (gui/label "Day: ") ;(->text "Day:") ;
          _   (add-watch quilsample.core/global-time :ticker
                        (fn [k r old new]
                         (.setText lbl (str "Day: " new))))
          _   (doto sw (.setPreferredSize (Dimension. 600 1000)))
          rootlayer  (.getLayer sw)
          
          [board dwells states fills chunks lbl :as panels] (mapv ->panel [(quilsample.core/board)
                                                                       (quilsample.core/dwells)
                                                                       (quilsample.core/state-trend-widget)
                                                                       (quilsample.core/fills)
                                                                       (quilsample.core/chunks 600 800)
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
      (do 
        (gui/toggle-top (gui/display-simple sw))
        (center! sw))))

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

;;animate entities...
(defn render!  [nd & {:keys [transform background handler]}]
    (let  [cnv (doto (->canvas)
                     (.setPreferredSize (java.awt.Dimension. 600 600)))
           layer (.getLayer cnv)
           _     (when transform (.setTransform layer transform))
           _     (when background (if (or (node? background)
                                          (satisfies? IPiccNode background))
                                    (add-child! layer (as-node nd))
                                    (.setPaint (.getCamera cnv) background)))]
      (add-child! layer (as-node nd))
      (when handler (.addInputEventListener layer handler))
      (show! cnv)))

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
