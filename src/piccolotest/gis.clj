;;Port of the mapboard implementation for
;;geospatial information systems from quilsample.
;;Serves as the basis for multiple node types,
;;including surfaces..
(ns piccolotest.gis
  (:require [piccolotest [sample :as    picc]
                         [timers :as t]]
            [clojure.core.async :as async :refer [ >!! <!! >! <!]]))

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

(defprotocol IParametric
  (time-reference [obj]))

;;can we merge these two?
;;Can we use a gis as an animated board?
;;Should be able to easily....
;;I think the GIS provides the basis for a lot of
;;our stuff...
;; (defprotocol IBoard
;;   (add-node  [b nd icon x y])
;;   (drop-node [b nd])
;;   (move-node [b nd x y])
;;   (get-layer [b]))

(comment ;;working on generalizing defining places.
;;Application Specific Data
;;=========================
(def worldmap    (picc/->image maps/bigmap {:id :worldmap :xscale 1.0 :yscale 1.0}))
(def world-nodes (vec (for [[id [x y]] maps/worldlocs]
                        (picc/->rect :black x y 10 10 {:id id :world-node id}))))
  
(defn ->map-node [nm img]  (picc/->image img {:id nm}))
;;ah.....note that we add a place to the map-node...
;;we identify it as being in the world.
(defn id-coords->places [xs]
  (vec (for [[id [x y]] id-coords]
         (picc/->rect :black x y 10 10 {:id id :world-node id}))))

(def usmap (picc/->image maps/usmap    {:id :usmap :xscale 0.14 :yscale 0.14}))
(def us-nodes
        (vec (for [[id [x y]] maps/uslocs]                    
                     (picc/->rect :black
                                  x y
                                  10 10 {:id id :us-node id}))))
)



;;____Functions for working on mapboards___
;;could be inlined...
;;Generic Operations
;;=================
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
        dx   (-  x (.getX bnds))
        dy   (-  y (.getY bnds))
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

;;Concerning arcs...
;;When we draw arcs, if the coords are in a different
;;layer, then we have to determine where to draw the arc,
;;if the arc is even visible.

;;currently, if the nodes are scaled and translated, the
;;bounds don't seem to be scaling and translating in turn.
;;Might be a bad node definition for scale and translate on my part.
(defn arc-between [nodes from to color]
  (let [[x y]    (get-coords nodes from)
        [x2 y2]  (get-coords nodes to)
        the-arc  (picc/->orientedCurve color x y x2 y2)]
      (picc/with-node-meta the-arc
        {:id [from to]
         :points (picc/flatten-path the-arc 1.0)})))

;;Generic.
(defn drop-arcs! [parent]
  (doseq [nd (filter (fn [nd] (vector? (:id (picc/node-meta nd))))
                     (picc/node-seq parent))]
    (let [p (.getParent nd)] (picc/drop-child! p nd))))
    
;;We can look into memoizing this in case it comes up a lot.

;;Computes a sequence of xy coords that gives us the path
(defn get-path [nodes from to]
  (:points (picc/node-meta  (arc-between nodes from to :black))))

(defn path-length [xs]
  (reduce + (map (fn [[l r]]
                   (apply picc/norm (picc/dist l r)))
                 (partition 2 1 xs))))

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

(defn color-token
  [nodes id clr]
  (picc/set-paint! (get-node nodes id) clr))

(defn jitter [[x y]]
  [(+ x (* (rand) 10))
   (+ y (* (rand) 10))])

;;Parametric Transitions (WIP)
;;============================
;;A fading arc is an example of an activity...
;;If we can lock down the PActivities so that we can
;;control them with channels/discrete time, that'd be nice.
(defn fading-arc [time-atom parent duration nodes from to color]
  (let [arc   (arc-between nodes from to color)
        alpha (atom 1.0)
        step  (/ 1.0 duration)
        t     @time-atom
        end-time (+ duration t)
        nd    (picc/->fade alpha arc)
        c     (async/chan 1)
        _     (picc/do-scene
               (picc/add-child parent nd)
               (t/add-timer time-atom duration (gensym "fade")
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

;;Might want to shift to an activity-based implementation later.
;;change this to a channel-based implementation.
;;get away from timers.
;;We're using invoke-later to ensure work is done on the swing EDT thread..
;;for both adding and removing arcs.  Still getting the odd null pointer
;;exception, dunno why, but I think we're okay.

;;we need to be able to cancel the send.
;;running into situations where we're changing
;;a unit's transit en-route.  What does it mean to
;;cancel?
;;Alter the path.
;;note: send-to depends on events.
(defn send-to
  "Send an icon - from - towards destination - to - using a provided speed.  Allows us 
   to also show the path of travel using an arc (or line).  We could also provide 
   an arg to set the stroke.  For now, we use solid arcs."
  [time-atom nodes from to & {:keys [speed arc? cancel? duration noise]
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
       
        t        @time-atom
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
        _       (t/add-timer time-atom (inc duration) send-nm
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
              (fading-arc time-atom (.getParent  from)
                          duration nodes from to :red)))
        ]
    res))

;;We can define tours, i.e. animated paths that a unit will travel at speed.
;;We may want to define speed as a function too..
(defn circuit [time-atom nodes from targets & {:keys [speed arc?] :or {speed 16 arc? true}}]
  (let [c (async/chan 100)]
    (async/go-loop [remaining targets]
      (if (seq remaining)
        (let [tgt  (first remaining)]
              (if-let [res (try (send-to time-atom nodes from tgt :speed speed :arc? arc?)
                            (catch Exception e
                              (do ;(async/>! errchan e)
                                  nil)))]
                (let [x (async/<! res)
                      _  (async/close! res)]
                  (do (async/put! c [tgt x]) ;await the movement
                      (recur (rest remaining))))
                (async/close! c)))
        (async/close! c)))
    c))

;;Tokens
;;======
;;Simple primitive tokens for placing things on the map.

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

;;Note: since we have multiple layers, we can have internal cameras...
(defrecord mapboard [tokens places top-layer map-layer arc-layer token-layer arcs? coords-fn]
  piccolotest.sample.IPiccNode
  (as-node   [nd]         top-layer)
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
  (get-coords [b k]         (coords-fn b k)) ;(coords* b k))
  (tokens     [b]           @tokens)
  (places     [b]           @places))

;;map constructor.
(defn make-map [map-layer  coords-fn & {:keys [places tokens arcs?]}]
  (let [token-layer (picc/as-node [])
        arc-layer   (picc/as-node [])
        top-layer   (picc/->cartesian [map-layer
                                       arc-layer
                                       token-layer])]
    (as-> (->mapboard (atom {}) (atom {}) top-layer map-layer arc-layer token-layer arcs? coords-fn) brd
      (reduce (fn [b tk]
                (add-token b (:id (picc/node-meta tk)) tk)) brd tokens)
      (reduce (fn [b [k nd]]
                (add-place b k nd)) brd places))))

