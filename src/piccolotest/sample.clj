(ns piccolotest.sample
  (:require [spork.cljgui.components.swing :as gui]
            [spork.graphics2d.canvas :as canvas]
            [spork.geometry.shapes :as shapes]
            [spork.graphics2d.swing :as swing]
            [spork.graphics2d.font :as font]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]])
  (:import
   [org.piccolo2d         PCanvas PLayer PNode PRoot POffscreenCanvas PCamera]
   [org.piccolo2d.event   PBasicInputEventHandler PDragEventHandler
                          PInputEvent PInputEventFilter]
   [org.piccolo2d.nodes   PPath PImage PText PShape]
   [org.piccolo2d.util   PBounds]
   [org.piccolo2d.extras.event
    PSelectionEventHandler
    PNotification
    PNotificationCenter]
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

;;These may be giving us grief, since we're not on the edt...
;;note: we could queue up children for removal on the edt..
;;rather than spam the edt with tons of child removal requests,
;;we could queue them up.
(defn add-child!  [^PNode p ^PNode c] (doto p (.addChild c)))
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
  org.piccolo2d.POffscreenCanvas
  (as-node   [nd]        (.getRoot ^POffscreenCanvas nd))
  (add-child [nd chld] (do (.addChild (.getLayer ^PCamera (.getCamera nd) 0) (as-node chld)) nd))  
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

(defn ^PNode invalidate! [^PNode nd]
  (doto nd
    (.invalidatePaint)))

(defprotocol MetaNode
  (node-meta [nd])
  (with-node-meta [nd m]))
                       
(extend-type org.piccolo2d.PNode
  MetaNode
  (node-meta [obj]         (.getAttribute obj "meta"))
  (with-node-meta [obj m]  (.addAttribute obj "meta" m) obj))

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

(defn ^PCanvas ->canvas
  ([]  (PCanvas.))
  ([& nodes] (reduce add-child (PCanvas.) nodes)))

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

;;probably make this something else...
(defn ^PNode translate
  [^PNode nd ^double x ^double y]
   (doto nd (.translate x (- y))))

(defn ^PNode translate-to!
  ([^PNode nd ^double x ^double y]
   (.setGlobalTranslation nd (java.awt.geom.Point2D$Double. x y)))
  ([nd ^clojure.lang.PersistentVector xy] (translate-to! nd (.nth xy 0) (.nth xy 1)))) 

(defn ^PNode translate-by! [^PNode nd ^double x ^double y ]
  (let [trans (doto (AffineTransform.)
                    (.translate (double x) (double (if *cartesian* (- y) y))))]
    (doto nd (.setTransform trans))))

(defn ^PNode scale! [^PNode nd  xscale yscale]
  (let [xscale  (double xscale)
        yscale  (double yscale)
        trans (doto (AffineTransform.)
                (.scale (double xscale) (double yscale)))]
    (doto nd (.transformBy trans))))

(defn ^PPath stroke! [^PPath nd ^java.awt.Stroke s]
  (doto nd
    (.setStroke s)))

;;I think we want this to be 0.0 for the x coordinate, not 1.0....
(defn ^PNode uncartesian! [^PNode nd]
  (let [height (.getHeight nd)]
    (-> nd 
        (translate! 0.0 height) ;altered from 1.0
        (scale! 1.0 -1.0))))
;(defn ^PNode scale! [^PNode nd ^double x ^double y] (doto nd (.scale x y)))


(defn node-children [^PNode nd]
  (iterator-seq (.getChildrenIterator nd)))

;;Search through the node meta data for ids
(defn find-node [id ^PNode root]
  (let [m (or (node-meta root) nil)]
    (if (= (get m :id) id) root
        (reduce (fn [acc nd]
                  (if (find-node id nd)
                    (reduced nd)
                    acc)) nil
                    (node-children root)))))

(defn node-seq [^PNode root]
  (tree-seq (fn [nd] (pos? (.getChildrenCount ^PNode nd)) )
            node-children root))
    

(defn node-map [^PNode nd]
  (into {}
        (filter (fn [[l r]]
                  l)
                (for [nd (node-seq nd)]
                  [(node-meta nd) nd]))))

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
      (add-child nd n))
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
      (add-child nd n))
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

;;rewrite using our node transforms.
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
      (add-child nd n))
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
      (add-child nd n))
    nd))


(defn ^PNode ->translate [x y child]
  (let [x  (double x)
        y  y ;(double (if *cartesian*  (- y) y))
        trans (doto (AffineTransform.)
                    (.translate (double x) (double y)))
        nd  (doto (PNode.) (.setTransform trans))]
     (add-child nd child)))

(defn ^PNode ->scale [xscale yscale child]
  (let [xscale  (double xscale)
        yscale  (double yscale)
        trans (doto (AffineTransform.)
                    (.scale (double xscale) (double yscale)))
        nd  (doto (PNode.)
              (.setTransform trans))
        ]
    (add-child nd child)))

(defn ->scaled-image [img xscale yscale & {:keys [id]}]
  (with-node-meta  
    (->scale xscale yscale
             (->image img))
    ((if id #(assoc % :id id) identity) 
     {:unscale [(/ 1.0 xscale) (/ 1.0 yscale)]      
      })))

(defn atom? [x] (instance? clojure.lang.Atom x))

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
                                        (invalidate! ^PNode nd)))))]
    (add-child  nd child)))

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
  (add-child (doto (PNode.) (.setTransform xform)) child))

;;In this sense, the piccolo scenegraph is no different from
;;the existing hud (it's not being interacted with the user).
(defn ^PPath ->ellipse [^double x ^double y ^double w ^double h]    (PPath/createEllipse x y w  h))
;(defn ^PPath ->line    [x y x2  y2]   (PPath/createLine    x y x2 y2))
(defn ^PNode ->text    [^String txt]  (uncartesian!       (PText. txt)))
                              
(def frame (atom nil))
(def canvas (atom nil))

(defn canvas? [x] (instance? org.piccolo2d.PCanvas x))

(defn show!
  ([cnv]
   (let [c (if  (canvas? cnv) cnv
                (doto (->canvas cnv)
                  (.setPreferredSize (java.awt.Dimension. 600 600))))
         _ (reset! canvas c)
         f (gui/toggle-top
            (gui/display-simple
            ;org.piccolo2d.extras.PFrame. "Canvas" false
             c))]
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
      (center! cnv)
      (show! cnv)))


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
      (when handler (.addInputEventListener layer handler))
;      (center! cnv)
      (show! cnv)))

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
      (when handler (.addInputEventListener layer handler))
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
      (when handler (.addInputEventListener layer handler))
      (center! cnv)
      cnv))
