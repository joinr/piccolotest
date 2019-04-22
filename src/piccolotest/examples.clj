(ns piccolotest.examples
  (:require [piccolotest [sample :as picc :refer :all]
                         [table :as tbl]]
            [spork.cljgui.components.swing :as gui]
            [piccolotest [events :as events]
                         [properties :as props]
                         [activities :as acts]]))


;;create a simple scene, with a stickied swing labeled button
;;that allows us to create blue dots when clicked.

;;we'd like to create canvases that have special properties.
;;namely, the ability to stick control widgets on a control layer....
;;control layer will follow the camera.
;;control layer will preserve its scale.
;;control layer will draw last.
(defn button-makes-dots []
  (let [random-circle! (fn [xmax ymax]
                         (->circle :blue (rand-int xmax) (rand-int ymax)
                                         30 30))
        circles    (->layer)  ;;a place for our circles to go...
        ;;a simple user interface...
        controls   (->button-strip {"Add!"
                                     (add-child circles
                                          (random-circle! 600 600))
                                     "Remove!"
                                         (do (pop-child! circles)
                                             (picc/invalidate! circles))})
        cnv      (render! [(->grid-lines (->filled-rect :red 0 0 600 600)) circles controls])
        _        (add-sticky cnv  controls)]
    cnv))

;;so, lets expand on our example.
;;It'd be cool if we had interactive zooming setup..
;;Let's implement that idiom.
;;So, we need to get the picked node.
;;And we need to animate to bounds.
;;And maybe have a way to go "back"
;;So, our UI paradigm now includes some general navigation
;;features...

;;[forward - zoom in to the picked node - implicit with 2x click]
;;[back - zoom back out iff applicable - implicit with up-left gesture]
;;[top  - zoom out to the top-level node, extents - implicit with toss-away gesture.]

(defn ->navigation []
  (let [path (atom [])]))
       
  

;;Activities example.
;;==================
(defn simple-test [& {:keys [dur] :or {dur 2000}}]
  (let [fr    (->filled-rect :red 0 0 100 100)
        cnv   (render! [(->filled-rect :white 0 0 1100 1100) fr])
        tl    (derive-timeline fr)
        clock (:clock @tl)        
        ;;creates something akin to a promise...a pending activity that
        ;;runs until time in clock = 20000
        _     (animate-to-position-scale-rotation fr 100 1000 1.0 0 dur tl)]
    (dotimes [i (/ dur 20.0)]
      (swap! clock (fn [x] (unchecked-add x 20)))
      (Thread/sleep 20))))
    
;;more complicated
(defn big-test [& {:keys [cached? n dur] :or {n 1000 dur 30000}}]
  (let [rects (for [i (range n)]
                (->filled-rect (java.awt.Color. (int (rand-int 255)) (int (rand-int 255)) (int (rand-int 255)))
                               (rand-int 1000)
                               (rand-int 1000)
                               20 20))
        rect-node (if cached? (->cache rects) rects)]
    (do (render! rect-node :render-options {:default :low
                                            :interacting :low
                                            :animating :low})
        (doseq [r rects]
          (animate-to-position-scale-rotation r (rand-int 1000) (rand-int 1000) 1.0 (/ Math/PI 2.0) dur))
        (let [clock (:clock @(derive-timeline (first rects)))]
          (dotimes [i (/ dur 20.0)]
            (swap! clock (fn [x] (unchecked-add x 20)))
            (Thread/sleep 20))))))

;;breaking up bounds by layers does appear to help.  There's likely a tradeoff
;;for the chunking size, and we can probably get some benefit if we
;;recompute bounds in parallel.  For now, we don't do that though.  doing fullbounds
;;over and over (maintaining the vbh) is just lots of reductions.  May need to
;;hack the PNode implementation to see if we can do something about it.
(defn big-test-partitioned [& {:keys [n dur size] :or {n 1000 dur 30000 size 100}}]
  (let [rects (for [i (range n)]
                (->filled-rect (java.awt.Color. (int (rand-int 255))
                                                (int (rand-int 255))
                                                (int (rand-int 255)))
                               (rand-int 1000)
                               (rand-int 1000)
                               20 20))
        rect-node (picc/->layer (for [xs (partition-all size rects)]
                                  (picc/->layer xs)))]
    (do (render! rect-node :render-options {:default :low
                                            :interacting :low
                                            :animating :low})
        (doseq [r rects]
          (animate-to-position-scale-rotation r (rand-int 1000) (rand-int 1000) 1.0 (/ Math/PI 2.0) dur))
        (let [clock (:clock @(derive-timeline (first rects)))]
          (dotimes [i (/ dur 20.0)]
            (swap! clock (fn [x] (unchecked-add x 20)))
            (Thread/sleep 20))))))

(defn big-test-sprites [& {:keys [n dur size] :or {n 1000 dur 30000 size 100}}]
  (let [sprites    (atom {})
        colors     [:red :green :blue :orange :light-red :light-green :light-blue
                    :violet :light-green]
        get-sprite (fn ([r g b]
                        (if-let [^org.piccolo2d.nodes.PImage itm (get sprites [r g b])]
                          (->image (.getImage itm))
                          (let [s  (->sprite (->filled-rect (java.awt.Color. (int r)
                                                                             (int g)
                                                                             (int b))
                                                            0
                                                            0
                                                            20 20))
                                _ (swap! sprites assoc [r g b] s)]
                            s)))
                     ([c]
                      (if-let [^org.piccolo2d.nodes.PImage itm (get sprites c)]
                        (->image (.getImage itm))
                        (let [s  (->sprite (->filled-rect c 0 0 20 20))
                              _  (swap! sprites assoc c s)]
                          s))))
        random-rect  (fn ([r g b x y]
                          (let [img (get-sprite r g b)]
                            (translate! img x y)))
                       ([c x y]
                        (let [img (get-sprite c)]
                          (translate! img x y))))
        rects (for [i (range n)]
                (random-rect (int (rand-int 255))
                             (int (rand-int 255))
                             (int (rand-int 255))
                             ;(rand-nth colors)
                             (rand-int 1000)
                             (rand-int 1000)))
        rect-node (picc/->layer (for [xs (partition-all size rects)]
                                  (picc/->layer xs)))]
    (do (render! rect-node :render-options {:default :low
                                            :interacting :low
                                            :animating :low})
        (doseq [r rects]
          (animate-to-position-scale-rotation r (rand-int 1000) (rand-int 1000) 1.0 (/ Math/PI 2.0) dur))
        (let [clock (:clock @(derive-timeline (first rects)))]
          (dotimes [i (/ dur 20.0)]
            (swap! clock (fn [x] (unchecked-add x 20)))
            (Thread/sleep 20))))))

;;Messing with tables.
;;====================
(defn random-col-color! [t]
  (tbl/do-col t (rand-int (tbl/col-count t))
          #(doseq [c %] (set-paint! c (rand-nth [:red :yellow :green :blue])))))

(defn rainbow-table [& {:keys [r c cached?] :or {r 20 c 20}}]
  (let [t (tbl/->table r c 200 200 nil)
        base (if cached?  (picc/->cache t) t)]
    (render! base)
    (dotimes [i 100000]
      (do-scene (random-col-color! t))
      (Thread/sleep 16))))
    

;;user-controlled timelines.
(defn timeline-example []
  (let [clock  (atom 0)
        timer  (atom nil)
        fr     (picc/->filled-rect :red 0 0 100 100)
        controls   (->button-strip 
                     {"Start!"  (reset! timer (future (while true 
                                                        (do (swap! clock + 20)
                                                          (Thread/sleep 20)))))
                      "Stop!"   (swap! timer 
                                  #(do (when (future? %) (future-cancel %))))})
        cnv     (picc/render! [fr #_controls]) ;;ensures it's rooted in a scene.
        _       (add-sticky cnv  controls)
        tl      (picc/derive-timeline fr clock)
        ;;queue up an activity, but nothing will happen unless we enable
        ;;time to flow!
        _       (picc/animate-to-position-scale-rotation fr 100 1000 1.0 0 2000 tl)]
    cnv))
    
;;use a user-defined timeline to control movement.
(defn big-test-tl [& {:keys [dur] :or {dur 30000}}]
  (let [rects (for [i (range 1000)]
                (picc/->filled-rect (java.awt.Color. (int (rand-int 255)) (int (rand-int 255)) (int (rand-int 255)))
                  (rand-int 1000)
                  (rand-int 1000)
                  20 20))]    
    (do (picc/render! rects)
      (doseq [r rects]
        (picc/animate-to-position-scale-rotation r 
          (rand-int 1000) (rand-int 1000) 1.0 (* Math/PI 2.0) dur))
      (let [clock (:clock @(picc/derive-timeline (first rects)))]
        (dotimes [i (/ dur 20.0)]
          (swap! clock (fn [x] (unchecked-add x 20)))
          (Thread/sleep 20))))))

;;level-of-detail nodes and semantic zoom via 
;;rendering point clouds that turn into grey boxes
;;past a certain zoom level.
(defn random-coords [w h n]
  (repeatedly n (fn [] [(rand-int w) (rand-int h)])))

(defn toggle-rect [big small x y w h]
  (if (identical? big small)
    (->filled-rect big x y w h)
    (->semantic-node (->filled-rect big x y w h)
      (->filled-rect small x y w h))))
(defn ->cloud
  ([n oncolor offcolor w h]
   (as-node (vec (for [[x y] (random-coords w h n)]
                   (toggle-rect oncolor offcolor x y 10 10)))))
  ([n color w h]  (->cloud n color color w h))     
  ([n w h] (->cloud n :green :blue w h))
  ([n] (->cloud n 600 600)))
  
  ;;nested-clouds....
  
  ;;so.....we can have an occluded container...
  ;;note:  I think primitives have a getBounds result....
  
  ;;each parent contains n children.
  ;;if level = 0, the children are regular clouds,
  ;;else,
  ;;  children are nested clouds....
  ;;  each child is scaled 0.01666 of the parent...
  ;;  so, at a given level, the child's scale is
  ;;  0.16 ^ level
(defn random-color [] (java.awt.Color. (int (rand-int 255))
                        (int (rand-int 255))
                        (int (rand-int 255))))
(defn nested-cloud [n level scale-factor init-scale x y w h]
  (if (zero? level)
    (->translate x y (->cloud (max (rand-int n) 1) 
                       (random-color)
                       (random-color)
                       w h))
    ;;translate and scale...      
    (let [current-scale  (* scale-factor init-scale)
          parent-color   (random-color)
          children (for [[x y] (random-coords w h (max (rand-int n) 1))]
                     (nested-cloud n
                       (unchecked-dec level)
                       scale-factor
                       current-scale
                       x y w h))]
      (->translate x y
        (->scale scale-factor scale-factor
          (->lod-box  0.8 (fn [x y w h]
                            (let [cx (+ x (/ w 2.0))
                                  cy (+ y (/ h 2.0))]
                              (as-node [(->circle parent-color
                                          cx
                                          cy  w h)
                                        (->translate  cx  (+ cy (/ h 2.0))
                                          (->scale 4.0 4.0 (->text  (str "Level : " level))))])))
            
            (as-node (vec children))))))))

;;semantic zooming test.
;;zooming in changes the color of the point cloud at a certain level,
;;zooming out far enough renders a grey block instead of the boxes.
(defn sem-test []
  (let [boxes  (->cloud 1000)
        bbox   (.getFullBounds boxes)
        block  (->filled-rect :grey (.getX bbox) (.getY bbox) (.getWidth bbox) (.getHeight bbox))]
    
    ;;we can create a node that toggles out at a zoom threshold to a grey box...
    (render! (->lod-box 0.25 block boxes))))


(defn sem-test-nested []
  (render! (->cartesian (nested-cloud 5  5 0.8 1 0 0  500 500))))
                

;;path testing
(defn path-test []
  (let [the-path   (->orientedCurve :black 0 0 200 200)
        the-points (flatten-path the-path 1)
        the-glyph  (->rect :red 0 0 10 10)
        update!    (follow-path! the-glyph 
                     (cycle (concat the-points (reverse the-points))) 15)]
    (picc/render! [the-path the-glyph])))

    

;;highlighting/selection/picking
;;highlighting
(defn highlighted-test []
  (render! (->cloud 600) :handler (highlighter :black)))


(comment
  (defn on-click! [nd f]  (with-input! nd {:mouseClicked  f}))
  (defn on-hover! [nd in out]
    (with-input! nd {:mouseEntered  in
                     :mouseExited   out}))
    
  (defn ->button [color x y w h label action]
    (let [state     (atom :up)          
          expanded  (as-node (->cartesian [(->filled-rect color x y w h)
                                           (->translate x y (->text label))]))]
      (on-click! expanded action)))
  
  (defn random-square [x y w h]
    (->filled-rect (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255))
      x y w h))
  (defn random-squares [n w h]    
    (for [i (range n)]    
      (random-square (* (rand) w) (* (rand) h) 50 50)))
  ;;note: this only works for node events.  If they're children,
  ;;the parent's never forward events to them.
  (defn ->hover-toggle [^PNode open ^PNode closed]
    (let [prior (atom nil)
          ep    (events/event-processor
                  :mouseEntered  (fn [e]
                                   (.setVisible open true)
                                   (.setVisible closed false))
                  
                  :mouseExited   (fn [e]
                                   (.setVisible open false)
                                   (.setVisible closed true)))
          _ (.setVisible closed false)]
      (doto (as-node [open closed])
        (.addInputEventListener ep))))
  
  (defn ->highlight-on-hover [color nd]
    (let [bnds (.getBounds nd)
          high (->rect color (.getX bnds) (.getY bnds) (.getWidth bnds) (.getHeight bnds))
          _    (.setVisible high false)
          ep   (events/event-processor
                 :mouseEntered  (fn [e]
                                  (.setVisible high true))
                 
                 :mouseExited   (fn [e]
                                  (.setVisible high false)))]
      
      (doto (as-node [nd high])
        (.addInputEventListener ep))))
  
  (defn ->click-toggle [^PNode open ^PNode closed]
    (let [prior (atom true)
          ep    (events/event-processor
                  :mouseClicked  (fn [e]
                                   (swap! prior not)
                                   (.setVisible open @prior)
                                   (.setVisible closed (not @prior))))
          
          _ (.setVisible closed false)]
      (doto (as-node [open closed])
        (.addInputEventListener ep))))
  
  (defn ->color-toggle [color nd]
    (let [prior (atom nil)
          ep (events/event-processor
               :mouseEntered  (fn [e]
                                (reset! prior (.getPaint nd))
                                (set-paint! nd color))
               :mouseExited   (fn [e]
                                (set-paint! nd @prior)))]
      (doto nd (.addInputEventListener ep))))
  
  (defn annotation [anno-node nd]
    (let [
          prior (atom nil)
          ep (events/event-processor
               :mouseEntered  (fn [e]
                                (reset! prior (.getPaint nd))
                                (set-paint! nd color))
               :mouseExited   (fn [e]
                                (set-paint! nd @prior)))]
      (doto nd (.addInputEventListener ep))))
  
  ;;so now we can interact...
  
  ;  (defn highlight! [border nd]
  ;    (let [bnds (.getBounds nd)
  
  (defn ->print-toggle [nd]
    (let [prior (atom nil)
          ep (events/event-processor
               :mouseEntered  (fn [e]                               
                                (println "entered!"))
               
               :mouseExited   (fn [e]
                                (println "Exited!"))
               
               :mouseMoved (fn [e] nil))]
      (doto nd (.addInputEventListener ep)))))
    
    
   


