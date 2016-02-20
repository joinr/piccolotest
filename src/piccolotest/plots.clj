;;trying to replicate the combinators for defining
;;dynamic plots, as nodes, rather than
;;as images.  Images aren't cutting it atm.
(ns piccolotest.plots
  (:require [piccolotest.sample :as p]
            [spork [sketch :as sketch] [trends :as trends] ]
            [spork.graphics2d.debug :as debug]
            [piccolotest.canvas :as canvas]))

(comment

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
    (shared/colored-point
     (if-let [clr (get-color tr)] clr :black))))

(defmulti get-plotter (fn [plot-type plot] plot-type))
(defmethod get-plotter :dot [plot-type {:keys [canvas xscale yscale get-color]}]
  (fn [[tr u v]]
    (canvas/push-shape canvas
        (sketch/uv->xy xscale yscale u v
             (->dot tr get-color)))))
  
(defmethod get-plotter :area [plot-type {:keys [canvas xscale yscale get-color]}]  
  (let [yinv (/ 1.0 yscale)]
    (fn [ts]
      (let [u  (:x @ts)]
        (canvas/push-shape canvas
            (sketch/uv->xy xscale yscale u 0.0  ;draws a vertical trend line at x 
                (sketch/scale 1.0 yinv
                        ts)))))))

;;Oh...plots are just sketches...
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

)

;;bad dependency on quilsample.plots
(defn simple-plt [& {:keys [width height series get-color]
                 :or {width 600 height 600 series [:a  :green
                                                   :b  :blue]}
                 get-color (fn [_] :red)}]
  (let [plt   (quilsample.plots/->dynamic-plot :title "the plot"
                                               :xlabel "x"
                                               :ylabel "y"
                                               :height height
                                               :width width
                                               :get-color get-color
                                               :name "blah" :xmax width :ymax height)
        plter (quilsample.plots/get-plotter :area plt)
        cnv   (p/->cartesian height (canvas/nodify (debug/shape->nodes (:plot plt))))
        data  (atom (spork.trends/trends-from series))
        push-slice (fn push-slice [x]
                     (let [nxt  (spork.trends/add-slice @data x)
                           _    (reset! data nxt)                   
                           ]
                       nxt))
        ^org.piccolo2d.PNode plotarea-node (p/find-node :plotarea cnv)
        add-sample (fn add-sample! [x]
                     (push-slice x)
                     (plter @data)
                     (.invalidatePaint plotarea-node))]        
    (p/with-node-meta cnv      
      (merge (p/node-meta cnv)
             plt
             {:plotter plt
              :data    data
              :push-slice push-slice
              :add-sample add-sample
              :plotarea-node plotarea-node
              :series series
              :width width
              :height height
             }))))

(defn clamp [l r x]
  (if (< x l) l
      (if (> x r) r
          x)))

(defn random-slices [series & {:keys [xmax ymax]}]
  (let [_ (println series)
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
  (let [{:keys [series width height xscale yscale add-sample plotarea-node]} (p/node-meta plt)
        next-slice! (random-slices series :xmax (* width xscale) :ymax (* height xscale))
        paint-state (object-array [plt nil])
        _ (println :plotting paint-state)]
    (do ;(swap! repaint-list conj paint-state)
        (future (while @alive
                  (when @plotting
                    (do (add-sample (next-slice!))
                        (aset paint-state 1 true)))
                  (Thread/sleep 16))))))

(defn n-plots [n]
  (let [
        the-plots (take n (repeatedly (fn [] (simple-plt))))
        _ (doseq [p the-plots] (plot-randomly! p))
        c (apply p/->stack the-plots)]
    (p/render! c)
    ))
