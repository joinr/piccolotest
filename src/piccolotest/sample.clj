(ns piccolotest.sample
  (:require [spork.cljgui.components.swing :as swing])
  (:import
   [edu.umd.cs.piccolo         PCanvas PLayer PNode PRoot]
   [edu.umd.cs.piccolo.event   PBasicInputEventHandler PDragEventHandler
                               PInputEvent PInputEventFilter]
   [edu.umd.cs.piccolo.nodes   PPath PImage PText]
   [edu.umd.cs.piccolo.util   PBounds]
   [java.awt Color Dimension Graphics2D]
   [java.awt.event   InputEvent MouseEvent]
   [java.awt.geom   Point2D]
   [javax.swing     JFrame]
   [java.util  ArrayList Random]))

;;This is a just a test space to work out some piccolo2d
;;apps.  Namely, to gain familiarity with the api,
;;and to figure out how we can wrap it in clojure easily.


;;THe basic workings of the piccolo2d api are frame(s), layers,
;;nodes, and a cameras.


;;the basis of everything is the frame.
;;Are there corresponding panels?
;;Can nodes be children of multiple layers?

;;PCanvas is a component, so we can use it like a jpanel.
(def my-canvas
  (doto 
      (PCanvas.)
    (.setPreferredSize (Dimension. 600 600))))

;;The canvas has a default or base layer.

;;Layer1 has a root, which is the default root associated with the PCanvas.
(def layer1 (.getLayer my-canvas))

;;this should be displayable as a jpanel.
;;we should be able to add a node.

;;One idea here is to use piccolo's event interface as a hookup
;;for view and rendering.  The nodes become part of the model.
;;As the simulation proceeds, we update the view by propogating
;;"movement" to the nodes. We basically simulate mouse movement
;;programmatically.

;;In this sense, the piccolo scenegraph is no different from
;;the existing hud (it's not being interacted with the user).
(defn ^PPath ->ellipse [x y w  h]  (PPath/createEllipse x y  w  h))
(defn ^PPath ->line    [x y x2 y2] (PPath/createLine    x y  x2 y2))
(defn ^PNode ->text [^String txt] (PText. txt))

;;like the grapheditor example, we have a layer for nodes and a
;;layer for edges.

;;The layer idiom allows us to partition visible items.

;;In addition to the base layer, we can define other layers.
;;layer2 has no root.  It doesn't exist outside of the canvas
;;context.  We have to specifically add it...
(def layer2 (PLayer.))
(do (.addChild (.getRoot layer1) layer2))
;;Now, both layers have the same root node.

;;addChild is used to add nodes to layers (and other nodes).

;;There is only one root I think.  It's the root node for the scene graph,
;;and defines traversals.
;;(.addChild (.getRoot   this)   edge-layer)
;;Cameras have layers they are designated to "look at".

;;The canvas has a camera setup by default.
;;The camera cannot see it (and thus won't render it / traverse
;;its nodes) until we add it into the hierarchy.
(do (.addLayer (.getCamera my-canvas) 0 layer2))

;;let's hello-world it all together....
;;layer1 is for shapes
;;layer2 is for text...

;;This lets us maintain a database of layers->nodes.
;;A decent way to examine and query our scene.
;;We can start to define higher-order functions on
;;scenes (like mapping, filtering, and other traversals).
;;Can we derive scenes from existing scenes?
(defn node-base (atom {}))

(defn add-shape! [nd]
  (.addChild layer1 nd))

(defn add-label! [nd]
  (.addChild layer2 nd))

(defn wipe-children! [nd])
(defn wipe-labels!   [])
(defn wipe-shapes!   [])

;;Note: the effect of layers rendered later is that
;;they overwrite earlier layers.
(add-label! (->text "Can you see me?"))

;;Also, the default is to set the node at the origin.

