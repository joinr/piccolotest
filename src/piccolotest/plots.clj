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

;;should be a drop-in replacement for dynamic-plot.
;;If we wanted to, we could place these in a separate
;;frame wrapped in a pcanvas, and have them interact
;;with swing without incident (I think).
;;Width and height define the viewable width and height, not necessarily the
;;dataset.  xmax and ymax define that..
(defn plot-node [plot-type & {:keys [name width height series get-color xmax ymax xlabel ylabel sliced]
                 :or {width 600 height 600 series [:a  :green
                                                   :b  :blue]}}]
  (let [get-color (if-let [gc get-color] gc  (apply hash-map series))
        sliced (or sliced (= plot-type :area))
        name (or name "the plot")        
        plt  (->dynamic-plot :title    name
                             :xlabel   (or xlabel "x")
                             :ylabel   (or xlabel "y")
                             :height    height
                             :width     width
                             :get-color get-color
                             :name "blah"
                             :xmax (or xmax width)
                             :ymax (or ymax height))
        plter (get-plotter plot-type plt)
        cnv   (p/->cartesian height (canvas/nodify (debug/shape->nodes (sketch/tag {:id name :class :plot-node}
                                                                                   (:plot plt)))))
                                        ;this only matters for dynamic area plots.
        ^org.piccolo2d.PNode plotarea (p/find-node :plotarea cnv)
        
        data       (when sliced (atom (spork.trends/trends-from series)))
        push-slice (when sliced
                     (fn push-slice [x]
                       (let [nxt  (spork.trends/add-slice @data x)
                             _    (reset! data nxt)                   
                             ]
                         nxt)))                       
        add-sample (if sliced (fn add-sample! [x]
                                (push-slice x)
                                (plter @data)
                                (.invalidatePaint plotarea))
                       (fn add-sample! [x]
                         (plter x)
                         (.invalidatePaint plotarea)))]        
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
              :sliced sliced}))))

;;this is just for testing..

(defn clamp [l r x]
  (if (< x l) l
      (if (> x r) r
          x)))

;;This is incompatible with dots...
(defn random-slices [series & {:keys [xmax ymax]}]
  (let [
        labels (mapv first (partition 2 series))
        x      (atom 0)
        series-values (zipmap labels (repeatedly (fn [] (atom (rand ymax)))))
        init-height (reduce + (map deref (vals series-values)))
        _ (when (> init-height ymax)
            (let [scale (double (/ ymax init-height 2.0))]
              (doseq [v (vals series-values)]
                (swap! v * scale))))
        stepsize (/ ymax 20.0)
        halfstep (* stepsize 0.5)
        step!  (fn [n]
                 (clamp 0 xmax (+ n (- (rand-int stepsize) halfstep))))]
    (fn []                              
      (if (> @x xmax) (reset! x 0) (swap! x unchecked-inc))
      (reduce-kv (fn [acc s v]
                   (let [res @v
                         _ (swap! v step!)]
                     (conj acc [s @x res]))) [] series-values))))
(def alive (atom true))
(def plotting (atom false))
(def repaint-list  (atom []))
(def paint-id (atom (rand)))
(def painter (future (while true
                       (when @plotting
                         (when-let [^org.piccolo2d.PCanvas f @p/canvas]
                           (.repaint f)))
                         ;; (swap! repaint-list (fn [xs]
                         ;;                       (reduce (fn [acc ^objects itm]
                         ;;                                 (if-let [cnv (aget itm 1)]                                                            
                         ;;                                   (do (.repaint ^org.piccolo2d.PCanvas (aget itm 0))
                         ;;                                       (aset itm 1 nil))
                         ;;                                   acc)) nil xs)))
                       (Thread/sleep 16))))
                                   

(defn plot-randomly! [plt]
  (let [{:keys [series width height xscale yscale add-sample plotarea-node plot-type sliced]} (p/node-meta plt)
        next-slice! (random-slices series :xmax (* width xscale) :ymax (* height xscale))
        paint-state (object-array [plt nil])                      
        _ (println :plotting paint-state)
        out *out*
        add-samples (if sliced (fn add-sliced-sample [] (add-sample (next-slice!)))
                        (fn add-samples [] (binding [*out* out]
                                             (doseq [x  (next-slice!)]
                                               (add-sample x)))))
                                 ]
    (do ;(swap! repaint-list conj paint-state)
      (future (while @alive
                (when @plotting
                  (do (add-samples) 
                      (aset paint-state 1 true)))
                (Thread/sleep 16)))
      )))

(defn n-plots [n & {:keys [plot-type] :or {plot-type :area}}]
  (let [
        the-plots (map (fn [idx] (plot-node plot-type :name (str "plot_" idx))) (range n))
        _ (doseq [p the-plots] (plot-randomly! p))
        c (apply p/->stack the-plots)]
    (p/render! c)
    ))
