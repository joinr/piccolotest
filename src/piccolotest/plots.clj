;;trying to replicate the combinators for defining
;;dynamic plots, as nodes, rather than
;;as images.  Images aren't cutting it atm.
(ns piccolotest.plots
  (:require [piccolotest.sample :as p]
            [spork.geometry.shapes :refer :all]
            [spork.graphics2d [debug :as debug] [image :as image]]
            [spork [sketch :as sketch] [trends :as trends] ]
            [spork.util.general]
            [piccolotest.canvas :as canvas]))
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
        ^org.piccolo2d.PNode plotarea (find-plot-area cnv)
        
        data       (when sliced (atom (spork.trends/trends-from series)))
        push-slice (when sliced
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

;;Icons are mobile nodes that live in a layer just above the
;;background of the plot.  Basically, we can inject dynamic
;;icons onto the plot, and move them dependently, or
;;independently of the plot.
(defn add-icon [nd nm icon & {:keys [x y] :or [x 0 y 0]}]
  (let [pa (:plotarea (p/node-meta nd))]
    (p/do-scene
     (p/add-child! pa icon)
;     (picc/translate! 
     )))

;;all we need is a layer with the entities added...
;;it acts very similar to the gis protocol...

;;helper fn to take advance of local->parent 
;; (defn ^java.awt.geom.Point2D$Double ->point [^double x ^double y]
;;   (java.awt.geom.Point2D$Double x y))

(comment
  (def ic (picc/->filled-rect :red 0 0 50 50))
  (def p  (piccolotest.plots/plot-node :dot :title "Blah" :width 600 :height 600 :series shared/risk-series :xmax 600 :ymax 600))
  (picc/add-child (:plotarea (picc/node-meta p)) ic)
  ;;we have the state stored...
  (def the-state  {:icons {:a [ic (atom [0 0])]}
                           :plot  p})
  (defn shift-origin [brd  nm]
    (let [[nd xy] (get-in brd [:icons nm])
          [x y]   @xy]
      (do (picc/translate! nd (- x) (- y))
          (reset! xy [0 0])
          brd)))
  (defn shift-entity [brd nm dx dy trail?]
    (let [[nd x0y0] (get-in brd [:icons nm])
          [x0 y0] @x0y0
          x (+ x0 dx)
          y (+ y0 dy)
          newxy [x y]]
      (do (when trail?
            (let [plt  (:plot brd)
                  add! (:add-sample (picc/node-meta plt))
                  clr  (quilsample.bridge/color->risk
                        (quilsample.bridge/default-cycle->color newxy))]
              (add! [clr x y])))
          (picc/translate! nd dx dy)
          (reset! x0y0 newxy)
          brd)))
  (defmacro msg [name & body]
    `(do ; (println ~name)
         ~@body))
  (defn step-entity [brd nm]
    (let [[nd x0y0] (get-in brd [:icons nm])
          [x0 y0] @x0y0
          ]
      (cond (pos? y0) ;;deployed, rise up.            
            (if (or (>= y0 600)    
                    (< (rand) 0.01))
              (msg "reset!" (shift-origin brd nm))
              (msg "bog"
                   (shift-entity brd nm 0 1 (> (rand) 0.75))))
            ;;deploy?
            (and (>= x0 300)
                 (< (rand) 0.001))
            (msg "Deploy!" (shift-entity brd nm 0 1 (> (rand) 0.7)))
            (>= x0 600)
               (msg "reset-nodep" (shift-origin brd nm))
            :else
               (msg "dwell"
                    (shift-entity brd nm 1 0 false)))))
                     
 ;;testing layout functions and stuff.. Trying to fix
 ;;node picking, which seems to be erroneous.
 (->dynamic-plot :title    "helloa"
                             :xlabel   "x"
                             :ylabel   "y"
                             :height    600
                             :width     600
                             :get-color (fn [_] :red)
                             :name "The-plot"
                             :xmax 600
                             :ymax 600)

  )
