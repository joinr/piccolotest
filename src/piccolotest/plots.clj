;;trying to replicate the combinators for defining
;;dynamic plots, as nodes, rather than
;;as images.  Images aren't cutting it atm.
(ns piccolotest.plots
  (:require [piccolotest.sample :as p]
            [spork.geometry.shapes :refer :all]
            [spork.graphics2d [debug :as debug] [image :as image]]
            [spork [sketch :as sketch] [trends :as trends] ]
            [spork.util.general]
            [piccolotest [canvas :as canvas] [gis :as gis]]))

;;Note: I'm currently using the plot specifications from
;;spork.sketch, and rendering them into scenes using
;;piccolotest.scene/nodify.  In the future, I will
;;port the plot and scale definitions from spork
;;into pure scene representations.
;;For now, we reuse them.

;;This is loosely shaping up into something akin to Grammar
;;of Graphics, albeit designed with an emphasis on realtime
;;i.e. animated plotting first.  For now, we'll stick with
;;the narrow specification for the plots that we have on hand.
;;Later, I'll work on fleshing it out into things like
;;facets and friends.  Note: we can implement the
;;facet functionality pretty easily using existing
;;clojure functions.

;;__Marks__
;;Marks - these would be geoms in GG
;;Copied from quilsample.shared to eliminate dep.
(def colored-ring (spork.util.general/memo-1
                   (fn [clr]
                     (spork.graphics2d.image/shape->img
                      :translucent (->ring clr 0 0 10 10)))))

(def colored-point (spork.util.general/memo-1
                    (fn [clr]
                      (spork.graphics2d.image/shape->img
                       :translucent (->circle clr 0 0 10 10)))))

(def colored-square
  (spork.util.general/memo-1
   (fn [clr]
     (spork.graphics2d.image/shape->img
      :translucent (->rectangle clr 0 0 10 10)))))

(def colored-pixel (spork.util.general/memo-1
                    (fn [clr]
                      (spork.graphics2d.image/shape->img
                       :translucent (->rectangle clr 0 0 5 5)))))

(defn ->colored-ring [clr x y]  (sketch/translate x y (colored-pixel clr)))
  
;;__Independent Plots__
;;Trying to break out stuff that's functionally independent.
;;It'd be nice to have a high-level api for composing charts and data streams.
;;Namely...
;;We have a trend stream (in the event stream) that pushes data along trends.
;;Interested parties (charts) observe alterations in the trends.
;;They update accordingly.


;;It may be useful to have a chart coordinator that maintains data flows to subordinate
;;charts/plots.  Communicates either via channels or atoms, and other such rap.
;;That'd let us use dynamic datasets ala Tableau or pivot charts, and from there, abstract
;;visualizations.

;;data is the dumping ground.  Could be a channel...Currently it's an atom.
;;Input tells us when to sample....i.e. from the tick, lets us know when changes
;;occur.  One we have an area plot, we can just shove data at it...
;;Ah...this allows us to abstract the implementation later, so we can have a dynamic plot,
;;or interact with a stateful plotting library.
;;note, this is actually a slice-plot, we draw slices at particular intervals, which over time
;;gives us an area chart effect.  slice-plot is inherently dynamic.

(defn ->dot [tr get-color]
  (sketch/fade 0.3
    (colored-point
     (if-let [clr (get-color tr)] clr :black))))

(defmulti get-plotter (fn [plot-type plot] plot-type))
(defmethod get-plotter :dot [plot-type {:keys [canvas xscale yscale get-color]}]
  (fn [[tr u v]]
    (spork.graphics2d.canvas/push-shape canvas
        (sketch/uv->xy xscale yscale u v
             (->dot tr get-color)))))
  
(defmethod get-plotter :area [plot-type {:keys [canvas xscale yscale get-color]}]  
  (let [yinv (/ 1.0 yscale)]
    (fn [ts]
      (let [u  (:x @ts)]
        (spork.graphics2d.canvas/push-shape canvas
            (sketch/uv->xy xscale yscale u 0.0  ;draws a vertical trend line at x 
                (sketch/scale 1.0 yinv
                        ts)))))))

(defn ->dynamic-plot [& {:keys [title xlabel ylabel
                                height width name xscale yscale xmin xmax ymin ymax get-color]
                         :or {xmin 0.0 ymin 0.0}}]
  (let [xscale      (or xscale (/  (- xmax xmin) width))
        yscale      (or yscale (/  (- ymax ymin) height))
        tplot       (sketch/->xy-trend-plot :height height
                                            :width  width)]
  {:xscale  xscale
   :yscale  yscale
   :canvas  tplot
   :plot  (sketch/->plot tplot
             :cached false
             :title  title
             :xlabel xlabel
             :ylabel ylabel
             :title-font  (spork.graphics2d.font/->font "ARIAL" [:bold] 20)
             :ylabel-font (spork.graphics2d.font/->font "ARIAL" [:bold] 22)
             :xlabel-font (spork.graphics2d.font/->font "ARIAL" [:bold] 22)
             :xmin xmin 
             :xmax xmax
             :ymin ymin
             :ymax ymax
             :plotxscale 1.0
             :plotyscale 1.0
             :h height
             :w width
             )
   :get-color get-color}))

(defn center [l r]
  (let [bounds (.getFullBounds r)]
    [(p/translate (/ (.getWidth bounds) 2.0) 0.0
                  l)
     r]))

(defn ->legend [series & {:keys [direction] :or {direction :vertical}}]
  (if (= direction :vertical)
    (apply p/->stack
           (for [[nm clr] (partition 2 series)]
             (let [txt (p/->text (str nm))
                   bounds (.getFullBounds txt)]          
               [(p/->filled-rect clr 0 0 10 (.getHeight bounds))
                (p/->translate  10 0 txt)
                ])))
    (apply p/->shelf
           (for [[nm clr] (partition 2 series)]
             (let [txt    (p/->text (str nm))
                   bounds (.getFullBounds txt)]          
               [(p/->filled-rect clr (/ (.getWidth bounds) 4.0) 0 (/ (.getWidth bounds) 4.0) 10)
                (p/->translate  0 10 txt)
                ]
                )))
    ))
;;this is a little hack to get our nodes lines up for the chart.
;;Note: we could institute alignment in the stack and shelf functions......
(defn middle-right [l r]
  (let [bnds (.getFullBounds l)
        h    (.getHeight bnds)
        mid  (/ h 2.0)
        left (.getX bnds)
        right (+ (.getX bnds) (.getWidth bnds))]    
    (let [bnds  (.getFullBounds r)
          xr    (.getX          bnds)
          hr    (.getHeight  bnds)
          ]
      [l
       (p/->translate  (- right xr)
                       (-        mid hr)
                      r)])))

;;this is a really terrible way to lookup the chart node....
(defn chart-node? [nd]
  (let [stroke? (fn [nd]
                  (= (:id (p/node-meta nd)) 'with-stroke))]
    (when-let [xs (p/node-children nd)]
      (and (p/image-node? (first xs))
           (every? stroke? (rest xs))
           ))))

(defn find-plot-area [root]
  (->> (p/node-seq root)         
       (some  #(when (= (:id (p/node-meta %)) :plotarea) %))))

;;should be a drop-in replacement for dynamic-plot.
;;If we wanted to, we could place these in a separate
;;frame wrapped in a pcanvas, and have them interact
;;with swing without incident (I think).
;;Width and height define the viewable width and height, not necessarily the
;;dataset.  xmax and ymax define that..
(defn plot-node [plot-type & {:keys [title width height series get-color xmax ymax xlabel ylabel sliced]
                 :or {width 600 height 600 series [:a  :green
                                                   :b  :blue]}}]
  (let [get-color (if-let [gc get-color] gc  (apply hash-map series))
        sliced    (or sliced (= plot-type :area))
        name      (or title "the plot")        
        plt  (->dynamic-plot :title    title
                             :xlabel   (or xlabel "x")
                             :ylabel   (or ylabel "y")
                             :height    height
                             :width     width
                             :get-color get-color
                             :name name
                             :xmax (or xmax width)
                             :ymax (or ymax height))
        plter (get-plotter plot-type plt)
        lgnd  (->legend series)
        cnv   (p/->cartesian height (middle-right
                                     ;for now, we cheat and use the 2d shape via nodify.
                                     (canvas/nodify (debug/shape->nodes (sketch/tag {:id name :class :plot-node}
                                                                                    (:plot plt))))
                                     lgnd))
                                        ;this only matters for dynamic area plots.
        ^org.piccolo2d.PNode plotarea  (find-plot-area cnv)
        plot-parent (p/node-parent plotarea)
        icon-layer  (p/->layer)
        _           (p/add-child plot-parent icon-layer)
        data        (when sliced (atom (spork.trends/trends-from series)))
        push-slice  (when sliced
                      (fn push-slice [x]
                        (let [nxt  (spork.trends/add-slice @data x)
                              _    (reset! data nxt)
                              ]
                          nxt)))
        ;;need to change this to work with piccolo.
        ;; add-sample (if sliced (fn add-sample! [x]
        ;;                         (push-slice x)
        ;;                         (plter @data)                                
        ;;                         (.invalidatePaint plotarea))
        ;;                (fn add-sample! [x]
        ;;                  (plter x)
        ;;                  (.invalidatePaint plotarea)))
        add-sample (if sliced (fn add-sample! [x]
                                (push-slice x)
                                (plter @data)
                                (p/do-scene 
                                 (.invalidatePaint plotarea)))
                       (fn add-sample! [x]
                         (plter x)
                         (p/do-scene 
                          (.invalidatePaint plotarea))))
        ]        
    (p/with-node-meta cnv      
      (merge (p/node-meta cnv)
             plt
             {:plotter plt
              :data    data
              :push-slice push-slice
              :add-sample add-sample
              :plotarea   plotarea
              :icon-layer  icon-layer
              :series series
              :width width
              :height height
              :plot-type plot-type
              :sliced sliced
              :wipe  (fn [] (spork.graphics2d.canvas/wipe (:plot plt)))}))))


;;trying to implement reset...
(defn clear-plot! [root]
  (->>  (p/find-node :plotarea root)
        (.getParent)
        (p/node-meta)
        (:plot)
        (spork.graphics2d.canvas/wipe)))

;;we can define operations for resizing too....
;;get dynamically resized plots if our bounds are exceeded...
;;#TODO
;;Dynamically resize plots according to data..


;;we can define a useful type of reusable plot here...
;;a trail-plot, where we basically have nodes in the 

;;so, a trail-plot acts like a map-node...
;;with the exception that we have a layer devoted to rendering
;;trails, or some other kind of dynamic content?

;;We basically have a fancy cartesian coordinate grid...
;;On top of the grid, we have a background layer (a 2d Texture,
;;or canvas, we can draw on with a default charting method),
;;and on top of that, we have dynamic nodes that move around
;;inside the plot.

(defn ->icon [id coords nd]
  {:id id
   :position coords
   :icon nd})

;;Icons are mobile nodes that live in a layer just above the
;;background of the plot.  Basically, we can inject dynamic
;;icons onto the plot, and move them dependently, or
;;independently of the plot.
(defn add-icons [nd xs]
  (let [pa  (:icon-layer (p/node-meta nd))]
    (p/do-scene
     (doseq [{:keys [id position icon]} xs]
       (let [[x y] position]
         (p/add-child pa icon)
         (p/translate! icon x y))))
    nd))
(defn add-icon [nd x] (add-icons nd [x]))

;;all we need is a layer with the entities added...
;;it acts very similar to the gis protocol...

;;helper fn to take advance of local->parent 
;; (defn ^java.awt.geom.Point2D$Double ->point [^double x ^double y]
;;   (java.awt.geom.Point2D$Double x y))

(defprotocol IScaled
  (xy->uv [o xy])
  (uv->xy [o uv]))

;;all plots should probably be iconic plots....  
(defrecord iconic-plot [icons plot-node add-sample xscale yscale]
  p/IPiccNode
  (as-node [this] plot-node)
  (add-child [this nd] (do (p/add-child plot-node nd)
                           this))
  IScaled
  ;;we need to inversely scale the shift...
  (xy->uv [brd [x y]]    
    [(* (/  1.0 xscale) x)
     (* (/  1.0 yscale) y)])
  (uv->xy [brd [u v]]    
    [(*  xscale u)
     (*  yscale v)])
  gis/IMapBoard
  (add-token  [b id tk]
    (if (map? tk) ;[nd xy] pair
      (let [{:keys [position icon]} tk
            [x y] position
            uv  (.xy->uv b position)  ;[(* x (/  1.0 xscale))
                                        ;(* y (/  1.0 yscale))]
            ]
        (iconic-plot. (assoc icons id [icon (atom position)])
                      (add-icon plot-node
                                (assoc tk :position uv))
                      add-sample
                      xscale
                      yscale
                      ))))
  (drop-token [b tk]  )
  (add-place  [b k nd])
  (drop-place [b k])
  (place-node [b k target])
  (get-node   [b k])
  (get-coords [b k])
  (tokens     [b] icons)
  (places     [b] nil))

(defn as-iconic [p]
  (let [{:keys [xscale yscale height width add-sample get-color wipe]} (p/node-meta p)]
    (->iconic-plot {} p add-sample xscale yscale)))

(defn add! [^iconic-plot  brd x y clr]
  (->> [clr x y]
       ((.add-sample brd))))

;;all we need is a way to compute the color from a point.
#_(defn shift-icon [^iconic-plot brd nm dx dy trail?]
  (let [[nd x0y0]  (get-in brd [:icons nm])
        [x0 y0]    @x0y0
        x (+ x0 dx)
        y (+ y0 dy)
        newxy      [x y]
        [du dv]    (xy->uv brd [dx dy])]
    (do (when trail?
          (let [clr  (quilsample.bridge/color->risk
                      (quilsample.bridge/default-cycle->color (xy->uv brd newxy)))]
            (add! brd  x y clr)))
        (p/translate nd du dv)
        (reset! x0y0 newxy)
        brd)))

(defn shift-icon [^iconic-plot brd nm dx dy]
  (let [[nd x0y0]  (get-in brd [:icons nm])
        [x0 y0]    @x0y0
        x (+ x0 dx)
        y (+ y0 dy)
        newxy      [x y]
        [du dv]    (xy->uv brd [dx dy])]
    (p/translate nd du dv)
    (reset! x0y0 newxy)
    brd))

(defn shift-origin [brd  nm]
  (let [[nd xy] (get-in brd [:icons nm])
                                        ;[x y]   @xy
        [u v]   (xy->uv brd @xy)
        ]
    (do (p/translate nd (- u) (- v))
        (reset! xy [0 0])
        brd)))

(defn icon-xy [brd id]
  @(second (get-in brd [:icons id])))

(defn icon-uv [brd nm] (xy->uv brd (icon-xy brd nm)))
(defn plot-by [^iconic-plot brd nm xy->clr]
  (let [xy (icon-xy brd nm)
        [x y] xy]
    (add! brd  x y (xy->clr xy))))
