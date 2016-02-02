;;implementation of a spork graphics2d canvas that
;;"draws" a nodes to a scenegraph.
(ns piccolotest.canvas
  (:require [spork.graphics2d.canvas :as canvas]
            [clojure.zip :as zip]
            [piccolotest.sample :as p])
  (:import [java.awt.Graphics2D]
           [org.piccolo2d         PCanvas PLayer PNode PRoot]
           [org.piccolo2d.nodes   PPath PImage PText PShape]
           [org.piccolo2d.util    PBounds]          
           [org.piccolo2d.extras.pswing PSwing PSwingCanvas]
           [org.piccolo2d.extras.nodes PNodeCache PLine]))


(defn tree-zip [tr]
  (zip/zipper map? :children
              (fn [{:keys [children] :as nd} xs]
                (assoc nd (into children xs)))  tr))

(defn leaves [tr]
  (let [ended     (atom nil)
        seen-end? (fn [loc]
                    (if @ended true
                        (do (reset! ended (zip/end? loc))
                            nil)))]                          
    (filter (fn [nd] (when (vector? nd) nd))
            (map first
                 (take-while (fn [loc]
                               (not (seen-end? loc)))
                             (iterate zip/next tr))))))



;;the goal is to convert instructions to nodes.
;;A flat set of nodes, with no real structure can be
;;immediately converted into the appropriate lines and such.
(defmulti nodify (fn [nd]
                   (let [res (cond (map? nd)    (first (:node nd))
                                   (vector? nd) (first nd)
                                   (extends? p/IPiccNode nd) :pnode
                                   :else (throw (Exception. (str ["unknown node type" nd]))))                         
                         ;_   (println res)
                         ]
                     res)))
(defmethod nodify :pnode [x]
  x)
(defmethod nodify :line   [[_ clr x y x2 y2]]
  (-> (p/->line clr x y x2 y2)
;      (p/set-paint! clr)
      ;(p/transform!  xform)
      ))

(defmethod nodify :string [[_ clr font s x y]]
  (-> (p/->text s)
     ; (p/set-paint! clr)
      (p/set-font! font)
      (p/translate! x y)
;      (p/transform!  xform)
      ))
(defmethod nodify :image  [[_  img transparency x y]]
  (-> (p/->image img)
      (p/translate! x y)
      ;(p/transform!  xform)
      ))

(defmethod nodify :rectangle  [[_  color x y w h]]
  (-> (p/->rect color x y w h)
;      (p/translate! x y)
      ;(p/transform!  xform)
      ))
(defmethod nodify :ellipse    [[_  color x y w h]]
  (-> (p/->circle color x y w h)
;      (p/translate! x y)
      ;(p/transform!  xform)
      ))
(defmethod nodify :circle    [[_  color x y w h]]
  (-> (p/->circle color x y w h)
;      (p/translate! x y)
      ;(p/transform!  xform)
      ))

(defmethod nodify :translate [{:keys [node children]}]
  (let [[_ x y] node]
    (p/->translate x y (mapv nodify children))))

(defmethod nodify :rotate [{:keys [node children]}]
  (let [[_ theta] node]
    (p/->rotate theta (mapv nodify children))))

(defmethod nodify :scale [{:keys [node children]}]
  (let [[_ xscale yscale] node]
    (p/->scale xscale yscale (mapv nodify children))))

(defmethod nodify :fade [{:keys [node children]}]
  (let [[_ alpha] node]
    (p/->fade alpha (mapv nodify children))))

(defmethod nodify :alpha [{:keys [node children]}]
  (let [[_ alpha] node]
    (p/->fade alpha (mapv nodify children))))

(defmethod nodify :begin [{:keys [node children]}]
  (mapv nodify children))

;;this could be a one-time operation, like setting the paint,
;;we just traverse all the children and set their strokes.
;;not sure how to handle this atm.
(defmethod nodify :stroke [{:keys [node children]}]
  (mapv nodify children) ;passthrough for now
  )

(comment 
;;testing
(defn
  simple-plt []
  (quilsample.plots/->dynamic-plot :title "the plot"
                  :xlabel "x"
                  :ylabel "y"
                  :height 600
                  :width 600
                  :get-color (fn [_] :red)
                  :name "blah" :xmax 600 :ymax 600))

(def  plt   (simple-plt))
(def  plter (quilsample.plots/get-plotter :area plt))
(def  cnv   (p/->cartesian 600 (nodify (spork.sketch/shape->nodes (:plot plt)))))
(def  data  (atom (spork.trends/trends-from [:a  :green
                                             :b :blue])))
(defn push-slice [x]
  (let [nxt  (spork.trends/add-slice @data x)
        _ (reset! data nxt)]
    nxt))

(defn clamp [l r x]
  (if (< x l) l
      (if (> x r) r
          x)))
(let [x (atom 0)
      a (atom (rand-int 600))
      b (atom (rand-int 600))]
  (defn plot-random! []
    (do 
      (if (> @x 600) (reset! x 0) (swap! x unchecked-inc))
      (swap! a (fn [n]
                 (clamp 0 600 (+ n (- (rand-int 200) 100)))))
      (swap! b (fn [n]
                 (clamp 0 600 (+ n ( - (rand-int 200) 100))))) 
      (push-slice [[:a @x @a] [:b @x @b]])
      (plter @data))))

)
;; (defn segments->sexp [xs]
;;   (reduce (fn [acc x]
;;             (if (atom? x)
;;               (conj acc x)
;;               (conj acc (seq (segments->sexp 
  
;;walk until we hit an opening node...
;;[n ........... ?]
;;[n prim|node ....]
;;[n prim ; continue walking, gathering prims]
;;[n [node ;; start walking again (recurse)] ]
;;[n [node ... close] ;;(return the current segment.)

;;so a segment is either..
;;[] -> empty
;;[prim] -> prim
;;[node prim* node] -> segment
;;[node prim|node* node]


(comment 

;;a render queue...
(deftype PiccGraphics [^Graphics2D g  width  height root]
  ICanvas2D
  (get-context    [cg]  cg)
  (set-context    [cg ctx] (throw (Exception. "not implemented")))  
  (draw-point     [cg color x1 y1 w]    
    (swap! instructions conj  [:point color x1 y1 w (get-transform cg)])
    cg)     
  (draw-line      [cg color x1 y1 x2 y2]
    (swap! instructions conj  [:line color x1 y1 x2 y2 (get-transform cg)])
     cg)
  (draw-rectangle [cg color x y w h]
    (swap! instructions conj
           [:rectangle color x y w h (get-transform cg)])
    cg)
  (fill-rectangle [cg color x y w h]
    (let [c  (if (nil? color) :black color)]
      (swap! instructions conj [:fill-rectangle color x y w h (get-transform cg)])
      cg ))
  (draw-ellipse   [cg color x y w h]
    (swap! instructions conj  [:ellipse color x y w h (get-transform cg)])
    cg)
  (fill-ellipse   [cg color x y w h]
    (swap! instructions conj [:fill-ellipse color x y w h (get-transform cg)])
    cg)
  (draw-string    [cg color font s x y]
    (swap! instructions conj [:string color font s x y (get-transform cg)])
    cg)  
  (draw-image [cg img transparency x y]
    (swap! instructions conj [:image img transparency x y (get-transform cg)])
    cg)
  IStroked
   (get-stroke [cg] (.getStroke g))
   (set-stroke [cg  s] (do (.setStroke g ^Stroke s)
                           (swap! instructions conj [:stroke s]))
     cg)
  ITextRenderer
  (text-width     [cg txt] (f/string-width (.getFont g) txt))
  (text-height    [cg txt] (f/string-height (.getFont g)  txt))
  (get-font       [cg] (.getFont g))
  (set-font       [cg f] (do (.setFont g ^Font f)
                             (swap! instructions conj  [:font f])
                             cg))
  ICanvas2DExtended
  (draw-polygon   [cg color points]
    (swap! instructions conj  [:poloygon color points (get-transform cg)])
    cg)
  (fill-polygon   [cg color points]
    (swap! instructions conj  [:fill-polygon color points (get-transform cg)])
    cg)
  (draw-path      [cg points] (not-implemented draw-path))
  (draw-poly-line [cg pline]  (not-implemented draw-poly-line))
  (draw-quad      [cg tri]    (not-implemented draw-quad))
  IBoundedCanvas
  (canvas-width   [c] width)
  (canvas-height  [c] height)
  IGraphicsContext
  (get-alpha      [cg] (get-composite g))
  (get-transform  [cg] (get-transform* g))
  (get-color      [cg] (get-current-color g))     
  (set-color      [cg c]
    (do (set-gui-color g 
                       (get-gui-color c))
        (swap! instructions conj  [:color c])
        cg))
  (set-alpha      [cg a]
    (do (set-composite g (make-alphacomposite a))
        (swap! instructions conj [:alpha a])
        cg))
  (set-transform  [cg t]
    (do (set-transform* g t)
        (swap! instructions conj  [:transform t])
        cg))  
  (translate-2d   [cg x y]
    (doto g (.translate (int x) (int y)))
    (swap! instructions conj  [:translate x y])
    cg)
  (scale-2d       [cg x y]
    (doto g (.scale  x  y))
    (swap! instructions conj  [:scale x y])
    cg)
  (rotate-2d      [cg theta]
    (doto g (.rotate (float theta)))
    (swap! instructions conj  [:rotate theta])
    cg)
  (set-state      [cg state] (interpret-state state g) cg)
  (make-bitmap    [cg w h transp] (make-imgbuffer w h transp) cg)
  clojure.lang.IDeref
  (deref [obj] {:g g :instructions instructions})
  ICartesian
  IPathable
  (get-path [obj] @instructions)
  )
)
