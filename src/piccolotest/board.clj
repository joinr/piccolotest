;;Provides a piccolo node for creating abstract boards with
;;name icons.  Also provides protocol functions for
;;manipulating the board (moving icons by name).
(ns piccolotest.board
  (:require [piccolotest.sample      :as    picc]
            [spork.graphics2d.canvas :as    canvas]))

;;another option is to draw to a nodecache, then blend that image.
;;We can maintain a raster....and the histogram of coordinates -> colors.
;;Another option is to just maintain a list of coordinate->color.
;;If it's encoded via bytes, we should be able to keep millions in memory.

;;Note:
;;We can encode trails by having a limited color set.
;;At any given point in time, we have a set of coordinates,
;;and the number of colors associated with them.
;;This way, we're just laying down trails over time.
;;We can recreate the trail at any point.
;;So, if we maintain a map of {coordinate -> {color -> count}}
;;Then 

;;we have scene-graphs from piccolo...
;;so, maybe we can present the entity layer separately from the
;;other layers.

;;we'll have a piccolo canvas to display our stuff onto.

;;normally, we'd have a canvas....
;;on that canvas (a SwingCanvas, typically gotten via PaintPanel), we'd
;;render everything in a cartesian system.
;;We draw to canvas and return the canvas every frame.
;;How's that work in a scenegraph?
;;Rather than drawing, we add the node.
;;We only really add the node once though.
;;Can we ensure the node already exists?
;;If it's mutable, we can maintain a node database.
;;Don't add the node if it exists...
;;Or, add the node, and "Drawing" implies that we draw like normal, except
;;we check to see if the bounds have changed.
;;We could just defer to the node's repaint method.

;;create an entity board.
;;We just want to display the entities moving around their positions.
;;That means, we create translated nodes for each entity.
;;The translation changes over time.
;;The easiest way to accomplish this is to directly translate
;;the nodes that represent each entity in a batch, and have
;;piccolo (or whatever) update all the moves.  Our goal is to have icons
;;placed in the appropriate places every turn.

;;Say we have multiple layers.
;;->Risk
;;->Chunks
;;->Map

;;Risk is our typical game board.
;;Rather than having a grid background, we just have the icons
;;there (for now).  We process updates by communicated movement (either
;;via a system or via changes).  For instance, when determining movement,
;;we can look for all the "moved" information in the CES, and update those entities
;;with their new positions.

(defprotocol IBoard
  (add-node  [b nd icon x y])
  (drop-node [b nd])
  (move-node [b nd x y])
  (get-layer [b]))

;;So, the basic api is starting to come to light.
;;We want the "map" to be able to
;;add/remove tokens (entities/units)
;;add/remove "places" -> mapping of a name to a node / coordinate.

;;it may be sufficient to simply extend the animated-board protocol to facilitate the
;;map's functionality.

;;__Animated Board Node__
;;An animated board is a data structure that maps entity names
;;to piccolo2d nodes, and maintains a piccolo2d root layer.
;;Its job is to maintain the (mutable, side-effecting) plumbing
;;for piccolo2d node management.

;;note, we're adding our negative translations up front.
(deftype animated-board [node-map board-layer]
  IBoard
  (add-node [ab nd icon x y]
      (let [node    (doto  (picc/->image (canvas/as-buffered-image icon :buffered-image)) ;this could be costing us in allocation.
                      (.translate (double x) (double
                                               y ;( - y)
                                              )))]
        (animated-board. 
         (assoc node-map nd node)            
         (picc/add-child! (if board-layer board-layer (picc/->layer)) node)
         )))
  (drop-node [b nd]
    (animated-board.
     (dissoc node-map nd)
     (picc/drop-child! board-layer nd)
     ))
  (get-layer [b] board-layer)
  (move-node [b nd x y] (do (picc/translate! (get node-map nd) (double x) (double y))))
  piccolotest.sample.IPiccNode
  (as-node [nd] board-layer)
  (add-child [nd child] (do (picc/add-child board-layer child) nd))
  clojure.lang.IDeref
  (deref [obj] {:node-map node-map :board-layer board-layer}))

(def empty-board (animated-board. {} nil))

;;__Animated Board Node__
;;An animated board is a data structure that maps entity names
;;to piccolo2d nodes, and maintains a piccolo2d root layer.
;;Its job is to maintain the (mutable, side-effecting) plumbing
;;for piccolo2d node management.

;;We're upping the ante now.  Boards can have places...

;;note, we're adding our negative translations up front.
(deftype animated-board [node-map board-layer]
  IBoard
  (add-node [ab nd icon x y]
      (let [node    (doto  (picc/->image (canvas/as-buffered-image icon :buffered-image)) ;this could be costing us in allocation.
                      (.translate (double x) (double
                                               y ;( - y)
                                              )))]
        (animated-board. 
         (assoc node-map nd node)            
         (picc/add-child! (if board-layer board-layer (picc/->layer)) node)
         )))
  (drop-node [b nd]
    (animated-board.
     (dissoc node-map nd)
     (picc/drop-child! board-layer nd)
     ))
  (get-layer [b] board-layer)
  (move-node [b nd x y] (do (picc/translate! (get node-map nd) (double x) (double y))))
  piccolotest.sample.IPiccNode
  (as-node   [nd] board-layer)
  (add-child [nd child] (do (picc/add-child board-layer child) nd))
  clojure.lang.IDeref
  (deref [obj] {:node-map node-map :board-layer board-layer}))

(def empty-board (animated-board. {} nil))
