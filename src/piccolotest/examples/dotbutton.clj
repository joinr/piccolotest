(ns piccolotest.examples
  (:require [piccolotest.sample :as sample]))

(def circles  (->layer))
(def controls (->layer [(gui/button "Howdy!" (fn [_] (add-child (base-layer @canvas) (->circle :blue (rand-int 600) (rand-int 600) 30 30))))]))
(render! [(->filled-rect :red 0 0 600 600) controls])
(->stick-to-camera controls (.getCamera @canvas))
;;we'd like to create canvases that have special properties.
;;namely, the ability to stick control widgets on a control layer....
;;control layer will follow the camera.
;;control layer will preserve its scale.
;;control layer will draw last.
