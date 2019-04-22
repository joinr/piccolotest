;;Generalization of 2d plotting of entities
;;on a 2d surface with reactive layers...
;;Stems from the development of interactive
;;mapping systems in zui.
(ns piccolotest.surface
  (:require [piccolotest.sample :as     picc]
            [piccolotest.sample :as     picc]
            [quilsample [maps :as maps] [events :as events]]
            [spork.entitysystem.store :as es]
            [spork.graphics2d.canvas  :as canvas]
            [clojure.core.async :as async :refer [ >!! <!! >! <!]]))

;;So, we have an image that probably has one or more regions we'd
;;like to act as containers.
;;easy way to do this is to have a nested tree of layers....

(defn dumb-example
  (let [background 

;;The aim here is to generalize the mapping system I built...
;;There is a correspondence between the 2d plot on the world map,
;;complete with transitional fading arcs, and the 2d plot of
;;entity icons moving across a surface, leaving "trails" when
;;they move north under a certain color...

;;It may be prudent to simply tag these as 2d plots....
;;We project the tokens onto the coordinate space and
;;optionally blend in the background image...

;;Then we get [plot background icons]
;;as a layer.


;;another option is to draw to a nodecache, then blend that image.
;;We can maintain a raster....and the histogram of coordinates -> colors.
;;Another option is to just maintain a list of coordinate->color.
;;If it's encoded via bytes, we should be able to keep millions in memory.


;;Note:
;;We can encode trails by having a limited color set.
;;At any given point in time, we have a set of coordinates,
;;and the number of colors associated with them.
;;This way, we're just laying down trails over time.
;;We can recreate the trail at any point.
;;So, if we maintain a map of {coordinate -> {color -> count}}
;;Then 

;;we have scene-graphs from piccolo...
;;so, maybe we can present the entity layer separately from the
;;other layers.

;;we'll have a piccolo canvas to display our stuff onto.

;;normally, we'd have a canvas....
;;on that canvas (a SwingCanvas, typically gotten via PaintPanel), we'd
;;render everything in a cartesian system.
;;We draw to canvas and return the canvas every frame.
;;How's that work in a scenegraph?
;;Rather than drawing, we add the node.
;;We only really add the node once though.
;;Can we ensure the node already exists?
;;If it's mutable, we can maintain a node database.
;;Don't add the node if it exists...
;;Or, add the node, and "Drawing" implies that we draw like normal, except
;;we check to see if the bounds have changed.
;;We could just defer to the node's repaint method.

;;create an entity board.
;;We just want to display the entities moving around their positions.
;;That means, we create translated nodes for each entity.
;;The translation changes over time.
;;The easiest way to accomplish this is to directly translate
;;the nodes that represent each entity in a batch, and have
;;piccolo (or whatever) update all the moves.  Our goal is to have icons
;;placed in the appropriate places every turn.

;;Say we have multiple layers.
;;->Risk
;;->Chunks
;;->Map

;;Risk is our typical game board.
;;Rather than having a grid background, we just have the icons
;;there (for now).  We process updates by communicated movement (either
;;via a system or via changes).  For instance, when determining movement,
;;we can look for all the "moved" information in the CES, and update those entities
;;with their new positions.

(defprotocol IBoard
  (add-node  [b nd icon x y])
  (drop-node [b nd])
  (move-node [b nd x y])
  (get-layer [b]))

;;possibly subsume the IBoard implementations with this guy.
(defprotocol IMapBoard
  (add-token  [b id tk])
  (drop-token [b tk])
  (add-place  [b k nd])
  (drop-place [b k])
  (place-node [b k target])
  (get-node   [b k])
  (get-coords [b k])
  (tokens     [b])
  (places     [b]))

;;So, the basic api is starting to come to light.
;;We want the "map" to be able to
;;add/remove tokens (entities/units)
;;add/remove "places" -> mapping of a name to a node / coordinate.

;;it may be sufficient to simply extend the animated-board protocol to facilitate the
;;map's functionality.


;;__Animated Board Node__
;;An animated board is a data structure that maps entity names
;;to piccolo2d nodes, and maintains a piccolo2d root layer.
;;Its job is to maintain the (mutable, side-effecting) plumbing
;;for piccolo2d node management.

;;We're upping the ante now.  Boards can have places...

;;note, we're adding our negative translations up front.
(deftype animated-board [node-map board-layer]
  IBoard
  (add-node [ab nd icon x y]
      (let [node    (doto  (picc/->image (canvas/as-buffered-image icon :buffered-image)) ;this could be costing us in allocation.
                      (.translate (double x) (double
                                               y ;( - y)
                                              )))]
        (animated-board. 
         (assoc node-map nd node)            
         (picc/add-child! (if board-layer board-layer (picc/->layer)) node)
         )))
  (drop-node [b nd]
    (animated-board.
     (dissoc node-map nd)
     (picc/drop-child! board-layer nd)
     ))
  (get-layer [b] board-layer)
  (move-node [b nd x y] (do (picc/translate! (get node-map nd) (double x) (double y))))
  piccolotest.sample.IPiccNode
  (as-node [nd] board-layer)
  (add-child [nd child] (do (picc/add-child board-layer child) nd))
  clojure.lang.IDeref
  (deref [obj] {:node-map node-map :board-layer board-layer}))

(def empty-board (animated-board. {} nil))

(comment 
;;entities on the board have a coordinate system relative to their parent node.
;;These are relative to a width and a height.  So, the local coordinate system
;;(the system of the children) is relative to the parent layer's system.
;;An alternative here is to add this all to the entity store, including icons,
;;and allow them to be unified.  So, the board ideally exists as part of the
;;entitystore (maybe not ideally, since it's really just for rendering...)
(defn entity-board [ces]
  (->>  (es/only-entities ces  [:name :position :state :color :icon]) ;;no te we're using reduce.        
        (reduce (fn [acc {:keys [position name icon]}]
                  (let [[x y] position]
                    (add-node acc name icon x y)))  empty-board)))
)

;;__side-effecting__
;;In theory, all the renderer has to do is update the nodes to ensure their
;;positioning is consistent.  The naive approach is to check everyone's x y coords.
;;If we embed the board (or the board's nodes) in the CES, we don't even need this.
;;Requires the ces to know what the position, velocity, and displacement are.
;;If we didn't have that, then we wouldn't explicitly need the store, only a
;;sequence of entities with the values already looked up...
;;so, in the parlance of CES, the entity renderer is a system that
;;operates on [the animated-board entity]
;;and         [entities with nodes on the board, which also have [:velocity :position :displacement]]
;;so, we could just say...
;;entities with [:velocity :position :displacement :node]

;;additionally, we only really want to perform updates if the board is visible, if
;;it's not, we can ignore the board and just run.
(defn draw-entities! [ces brd]
  (do (reduce-kv (fn [acc nm ^org.piccolo2d.PNode nd]
                   (let [;[^double x ^double y]   (gete ces nm :position)                         
                         [^double dx ^double dy]       (es/gete ces nm :velocity)
                         [x y]                         (es/gete ces nm :position)                         
                         [^double xoff ^double yoff]   (or (es/gete ces nm :displacement)
                                                           [0.0 0.0])                         
                         endx (+  dx xoff)
                         endy (+  dy yoff) 
                         ]
                     (do (picc/translate nd endx endy) 
                         (.invalidatePaint nd)                         
                     acc)))
                 nil
                 (:node-map @brd))
      ces))


;;___Map-board___
;;This is more sophisticated than the animated-board, we may
;;want to retrofit the AB as a map-board.
(defn shift-node! [^org.piccolo2d.PNode nd ^double x ^double y]
  (do (picc/translate nd x y) 
      (.invalidatePaint nd)
      nd)) 

(def worldmap (picc/->image maps/bigmap {:id :worldmap :xscale 1.0 :yscale 1.0}))

(def world-nodes (vec (for [[id [x y]] maps/worldlocs]
                       (picc/->rect :black x y 10 10 {:id id :world-node id}))))

(def usmap (picc/->image maps/usmap    {:id :usmap :xscale 0.14 :yscale 0.14}))
(def us-nodes
        (vec (for [[id [x y]] maps/uslocs]                    
                     (picc/->rect :black
                                  x y
                                  10 10 {:id id :us-node id}))))

(defn ->grid-lines [^org.piccolo2d.PNode nd]
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
             (picc/->line :black x (+ y (* n vstep)) width (+ y (* n vstep))))
           (for [n (range 11)]
             (picc/->line :black (+ x (* n hstep)) y  (+ x (* n hstep)) height ))))))

(defn ->background [clr nd]
  (let [bounds (.getFullBounds nd)
        height (.getHeight bounds)
        width  (.getWidth bounds)
        x      (.getX bounds)
        y      (.getY bounds)]
    [(picc/->rect clr x y width height)
     nd]))

;;These are visually appealing defaults for embedding the
;;state map within the world map.  Currently, my transformations
;;are not reporting correctly, so I am resorting to using the
;;translation and scaling information directly.  I hope to
;;not do this in the future.
(def ^:constant +us-x+ 250)
(def ^:constant +us-y+ 675)
(def ^:constant +us-xscale+ 0.14)
(def ^:constant +us-yscale+ 0.14)
(def ^:constant +icon-size+ 10)

;;I just kind of eyeballed this composition...
;;This produces a composite map layer.
;;We actually gave a sub-map, the continental us, nested within
;;the global map.
(defn composite-map
  []
  (picc/as-node [ [(->grid-lines worldmap)
                   world-nodes]
                  (picc/->translate +us-x+ +us-y+
                                    (->background :white (picc/->scale +us-xscale+ +us-yscale+
                                                                       [usmap
                                                                        us-nodes])))]))

;;____Functions for working on mapboards___
;;could be inlined...
(defn coords* [nodes nd & {:keys [yscale yoffset xscale xoffset]
                          :or {yscale +us-yscale+
                               yoffset +us-y+
                               xscale +us-xscale+
                               xoffset +us-x+}}]
  (if (vector? nd) nd
      (let  [
             to-node     (get-node nodes nd)
             to-bounds   (.getFullBoundsReference
                          ^org.piccolo2d.PNode to-node)
             ]
        (if (get (picc/node-meta to-node) :us-node)
          [ ;(.getX to-bounds)
           (+ (* xscale (.getX to-bounds)) xoffset)
           (+ (* yscale (.getY to-bounds)) yoffset)]
          [(.getX to-bounds)
           (.getY to-bounds)]))))

;;Shift is our general purpose tool for moving nodes "intuitively".
;;We have instances of scaled icons, which store their information
;;as attributes in the node meta data.  When we shift (globally),
;;we need to apply the scaling factor to the translation.
;;This will probably incur some cost for calls to nodemeta and
;;lookup, however, we can always create a custom scaled-image node
;;if necessary.
(defn shift! [nd x y]
  (if-let [m (picc/node-meta nd)]
    (if-let [^clojure.lang.PersistentVector unscale (get m :unscale)]
      (picc/translate! nd (* x (.nth unscale 0))
                          (* y (.nth unscale 1)))
      (picc/translate! nd x  y))
    (picc/translate! nd x  y)))

(defn shift-to! [^org.piccolo2d.PNode nd x y]
  (let [bnds (.getFullBoundsReference nd)
        dx   (- x (.getX bnds))
        dy    (-  y (.getY bnds))
        ]
    (if-let [m (picc/node-meta nd)]
      (if-let [^clojure.lang.PersistentVector unscale (get m :unscale)]
        (picc/translate! nd (* dx (.nth unscale 0))
                         (* dy (.nth unscale 1)))
        (picc/translate! nd dx  dy))
      (picc/translate! nd dx  dy))))
  
  
;;immediately moves the node to a target that is either a known node
;;or a destination in the node map
(defn place [nodes nd target]
  (let [nd    (get-node nodes nd)
        [x y] (get-coords nodes nd)
        [x2 y2] (if (vector? target) target (get-coords nodes target))]
    (shift! nd (- x2 x)
            (- y2 y)
            )))
  
;;currently, if the nodes are scaled and translated, the
;;bounds don't seem to be scaling and translating in turn.
;;Might be a bad node definition for scale and translate on my part.
(defn arc-between [nodes from to color & {:keys [yscale yoffset]
                                          :or {yscale +us-yscale+
                                               yoffset +us-y+}}]
  (let [[x y] (get-coords nodes from)
        [x2 y2] (get-coords nodes to)
        the-arc  (picc/->orientedCurve color x y x2 y2)]
      (picc/with-node-meta the-arc
        {:id [from to]
         :points (picc/flatten-path the-arc 1.0)})))

;;Might want to shift to an activity-based implementation later.
;;change this to a channel-based implementation.
;;get away from timers.
;;We're using invoke-later to ensure work is done on the swing EDT thread..
;;for both adding and removing arcs.  Still getting the odd null pointer
;;exception, dunno why, but I think we're okay. 
(defn fading-arc [parent duration nodes from to color & {:keys [yscale yoffset]
                                                  :or {yscale +us-yscale+
                                                       yoffset +us-y+}}]
  (let [arc   (arc-between nodes from to color :yscale yscale :yoffset yoffset)
        alpha (atom 1.0)
        step  (/ 1.0 duration)
        t     @events/global-time
        end-time (+ duration t)
        nd    (picc/->fade alpha arc)
        c     (async/chan 1)
        _     (picc/do-scene
                 (picc/add-child parent nd)
                 (events/add-timer duration (gensym "fade")
                     (fn [t]
                       (if (< t end-time)                                    
                         (swap! alpha (fn [x] (max (- x step) 0.0)))
                                        ;(picc/drop-child! parent nd)
                         (when (async/put! c true)
                           (do ;(picc/set-visible! nd false)
                             (async/close! c))                                        
                           )
                         
                         ))))  
        _     (async/go
                (let [res (<! c)]
                  (picc/do-scene
                   (picc/drop-child! parent nd)                 
                   )
                   ))
        ]
    nd))

(defn drop-arcs! [parent]
  (doseq [nd (filter (fn [nd] (vector? (:id (picc/node-meta nd))))
                     (picc/node-seq parent))]
    (let [p (.getParent nd)] (picc/drop-child! p nd))))
    
;;We can look into memoizing this in case it comes up a lot.

;;Computes a sequence of xy coords that gives us the path
(defn get-path [nodes from to]
  (:points (picc/node-meta  (arc-between nodes from to :black))))

(defn path-length [xs]
  (reduce + (map (fn [[l r]] (apply picc/norm (picc/dist l r))) (partition 2 1 xs))))

(defn get-length
  [nodes from to]
  (path-length (get-path nodes from to)))


;;applies random noise to the x and y coords.
(defn jitter-by
  ([width x y]
   (let [hw (/ width 2.0)
         w  (- (* (rand) width) hw)
         h  (- (* (rand) width) hw)]
     [(+ x w) (+ y h)]))
  ([width [x y]] (jitter-by width x y)))

;;we need to be able to cancel the send.
;;running into situations where we're changing
;;a unit's transit en-route.  What does it mean to
;;cancel?
;;Alter the path.
(defn send-to
  "Send an icon - from - towards destination - to - using a provided speed.  Allows us 
   to also show the path of travel using an arc (or line).  We could also provide 
   an arg to set the stroke.  For now, we use solid arcs."
  [nodes from to & {:keys [speed arc? cancel? duration noise]
                    :or {speed 16 arc? true}}]
  (let [^org.piccolo2d.PNode from           (get-node nodes from)
        ;;allows us to provide a jitter factor.
        to         (if noise
                     (let [nd (get-node nodes to)]
                       (->> (get-node nodes to)
                            (get-coords nodes)
                            (jitter-by noise)))
                     to)
        the-path     (get-path nodes from to)
       
        t        @events/global-time
        dist     (get-length nodes from to)
        speed    (if duration  (/  dist duration) speed)
        duration (or duration (/ dist speed)) ;;we could make duration an atom and let timer watch.
        end-time (+ duration t)
        step         (picc/follow-path!  from
                                         the-path
                                         speed
                                         shift-to!) 
        res      (async/chan (async/dropping-buffer 1)) ;ensure we don't have a problem.
        ;;maybe use channels here instead.
        send-nm (gensym "send")
        _       (events/add-timer (inc duration) send-nm
                                  (if cancel?
                                    (fn [t]
                                      (if (cancel?)
                                          (do 
                                            (async/put! res t)
                                            (async/close! res))
                                          (if-let [nxt (picc/do-scene (step  1))]
                                            nil
                                            (do 
                                              (async/put! res t)
                                              (async/close! res)))))
                                    (fn [t]
                                      (if-let [nxt (picc/do-scene (step  1))]
                                        nil
                                        (do 
                                          (async/put! res t)
                                          (async/close! res))))))                             
        _  (when arc?
             (picc/do-scene
              (fading-arc (.getParent  from)
                          duration nodes from to :red)))
        ]
    res))

  
(defn check!! [c]
  (let [t (async/timeout 30)]
    (let  [[v p]  (async/alts!! [t c])]
      (if (= p t) :timeout
          v))))

(defn check! [c]
  (let [t (async/timeout 30)]
    (let [[v p] (async/alts! [t c])]
      (if (= p t) :timeout
          v))))

;;This works well for repl-driven development using goroutines
;;and threads.

(def errchan (async/chan 100))
(let [out *out*]
  (async/go
    (binding [*out* out]
      (loop []
        (when-let [res (async/<! errchan)]
          (do (println  res)
              (recur)))))))      

;;We can define tours, i.e. animated paths that a unit will travel at speed.
;;We may want to define speed as a function too..
(defn circuit [nodes from targets & {:keys [speed arc?] :or {speed 16 arc? true}}]
  (let [c (async/chan 100)]
    (async/go-loop [remaining targets]
      (if (seq remaining)
        (let [tgt  (first remaining)]
              (if-let [res (try (send-to nodes from tgt :speed speed :arc? arc?)
                            (catch Exception e
                              (do (async/>! errchan e)
                                  nil)))]
                (let [x (async/<! res)
                      _  (async/close! res)]
                  (do (async/put! c [tgt x]) ;await the movement
                      (recur (rest remaining))))
                (async/close! c)))
        (async/close! c)))
    c))
  
;;push the icon along the points dictated by the path in the arc.
;;icon moves according to speed.  As time changes, we
;;update position of the icon.  It moves toward nodes sequentially.
  
(defn ->token [id img color & {:keys [label]}]
  (let [icon (picc/->scaled-image img 0.2 0.2 :parent id)
        bnds (.getFullBounds ^org.piccolo2d.PNode icon)
        w    (.getWidth  bnds)
        h    (.getHeight bnds)
        nd   (picc/->filled-rect color
                          0 0  
                          (* 2 w) h {:id id})
        res (picc/add-child nd icon)]
    (if label
      (picc/add-child res (picc/->text (str label)))
      res
      )))

(defn color-token
  [nodes id clr]
  (picc/set-paint! (get-node nodes id) clr))

(defn jitter [[x y]]
  [(+ x (* (rand) 10))
   (+ y (* (rand) 10))])

 
;;It'd be nice to maintain regions.
;;That way, we can do region queries and
;;place tokens "in" the region rather than
;;"at" a specific place.  Further, if the
;;node is identified as a region, we can
;;detect that in send-to, and allow some
;;discretion in where the unit is actually
;;placed.  Probably need a way to pack nodes
;;in the region.  Like, define slots
;;that map to coordinates.  From there,
;;push nodes into slots.  Or, just
;;have nodes move in a random direction
;;until they are in a region and not overlapping
;;other nodes.  Could be a simple force
;;computation model that applies shifts
;;to nodes incrementally to ensure they're
;;laid out in a pleasing manner.  We can use
;;regions to define containment too.  So we can
;;visualize containers.
(defrecord mapboard [tokens places top-layer map-layer arc-layer token-layer arcs?]
  piccolotest.sample.IPiccNode
  (as-node [nd] top-layer)
  (add-child [nd child]   (do (picc/add-child top-layer child) nd))  
  IMapBoard
  (add-token  [b  id  tk] (do (swap! tokens assoc id tk)
                              (swap! places assoc id tk)
                              (picc/add-child token-layer tk)
                              b))                                                            
  (drop-token [b   tk]    (do (swap! tokens dissoc (:id (picc/node-meta tk)) tk)
                              (swap! places dissoc (:id (picc/node-meta tk)))
                              ;;NOTE: This could be our memory leak!, looks like it isn't the culprit..
                              (picc/drop-child! token-layer tk)
                              b))
  (add-place  [b k nd]      (do (swap! places assoc k nd) b))
  (drop-place [b k]         (do (swap! places dissoc k) b))
  (place-node [b k target]  (do (place b k target) b))
  (get-node   [b nd]        (cond (picc/node? nd) nd
                                  (vector? nd) nd
                                  :else
                                  (if-let [res (get @places nd)]
                                    res
                                    (throw (Exception. (str "node " nd " does not exist in the node map"))))))
  (get-coords [b k]         (coords* b k))
  (tokens     [b]           @tokens)
  (places     [b]           @places))

;;map constructor.
(defn make-map [map-layer  & {:keys [places tokens arcs?]}]
  (let [token-layer (picc/as-node [])
        arc-layer   (picc/as-node [])
        top-layer   (picc/->cartesian [map-layer
                                       arc-layer
                                       token-layer])]
    (as-> (->mapboard (atom {}) (atom {}) top-layer map-layer arc-layer token-layer arcs?) brd
      (reduce (fn [b tk]
                (add-token b (:id (picc/node-meta tk)) tk)) brd tokens)
      (reduce (fn [b [k nd]]
                (add-place b k nd)) brd places))))

(defn empty-map [& {:keys [ arcs?] :or { arcs? true}}]
  (let [the-map (composite-map)
        places  (reduce-kv (fn [acc k v]
                             (assoc acc (:id k) v)) {} (picc/node-map the-map))
         ]                 
    (make-map the-map :places places :arcs? arcs?)))


;;Aux functions
(def us-locations   (vec (keys maps/uslocs)))
(defn random-state [] (rand-nth us-locations))
(def world-locations (vec (keys (dissoc maps/worldlocs "USA"))))
(defn random-region [] (rand-nth world-locations))


;;Old testing code...
(comment ;testing
  (require 'quilsample.shared)
  (defn flip [icon & {:keys [w h] :or {w +icon-size+ h +icon-size+}}]
     (spork.sketch/iconify (:data icon)
                           :w 50 :h 50 :flipped? true))
  
  (def sicon         (spork.sketch/iconify quilsample.shared/sbct :w 50 :h 50 :flipped? false))
  (def entities      (atom {}))
  (defn ->entity     [icon color]
  (let [id  (count @entities)
        tk (->token id sicon (rand-nth [:green :blue :orange :yellow])
                                        ;:label id
                    )]
    (swap! entities (fn [m] (assoc m id tk)))
    tk))
  (def locs          (keys quilsample.maps/uslocs))
  (def starting-locs (vec (keys quilsample.maps/uslocs)))
  (def destinations  (vec (keys (dissoc quilsample.maps/worldlocs "US"))))
                                                 
    
  (def nds (into {} (for [n (range 500)]
                      (let [e (->entity sicon (rand-nth [:yellow :green :orange]))
                            id (get (picc/node-meta e) :id)]
                        [n e]))))
  
  ;)
  (def the-map (picc/->cartesian [(composite-map)
                                  (vec (vals nds))]))


  
   ;;this doesn't preserve cart.  (do (picc/add-child the-map ))
  (def nm (reduce-kv (fn [acc k v]
                       (assoc acc (:id k) v)) {} (picc/node-map the-map)))

    
  
  (def the-path (path-between nm "TX" "SA"))
  (def step     (picc/follow-path! nd
                                   (concat the-path
                                           (reverse the-path))
                                   1))
  

   
)
