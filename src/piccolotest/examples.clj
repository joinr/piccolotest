(ns piccolotest.examples
  (:require [piccolotest.sample :as picc :refer :all]
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

