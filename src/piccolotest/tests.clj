(ns piccolotest.tests
  (:require [piccolotest.plots :refer :all]
            [piccolotest.sample :as p]))

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
                                               (add-sample x)))))]
                                 
    (do ;(swap! repaint-list conj paint-state)
      (future (while @alive
                (when @plotting
                  (do (add-samples) 
                      (aset paint-state 1 true)))
                (Thread/sleep 16))))))
      

(defn n-plots [n & {:keys [plot-type] :or {plot-type :area}}]
  (let [
        the-plots (map (fn [idx] (plot-node (rand-nth [:area :dot])
                                            :name (str "plot_" idx) 
                                            :series [:a (rand-nth [:red :green :blue])                                                     
                                                     :b (rand-nth [:orange :yellow :purple])]))
                       (range n))
        _ (doseq [p the-plots] (plot-randomly! p))
        c (apply p/->stack the-plots)]
    (p/render! c)))
    
