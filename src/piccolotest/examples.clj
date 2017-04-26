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
        _        (->stick-to-camera controls (active-camera cnv))]
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
  (let [path (atom [])
       ])
  )

;;Activities example.
;;==================
(defn simple-test [& {:keys [dur] :or {dur 2000}}]
  (let [fr    (->filled-rect :red 0 0 100 100)
        cnv   (render! fr)
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
                              _ (swap! sprites assoc c s)]
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
      (Thread/sleep 16))
    ))

;;Investigate - lazy nested rainbow tables...
    

