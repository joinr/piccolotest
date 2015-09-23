(ns piccolotest.GraphEditor
  (:import
   (org.piccolo2d   PCanvas PLayer PNode PRoot)
   (org.piccolo2d.event   PBasicInputEventHandler PDragEventHandler
                               PInputEvent PInputEventFilter)
   (org.piccolo2d.nodes   PPath)
   (org.piccolo2d.util   PBounds)
   (java.awt Color Dimension Graphics2D)
   (java.awt.event   InputEvent MouseEvent)
   (java.awt.geom   Point2D)
   (javax.swing   JFrame)
   (java.util   ArrayList Random)
   ))
  
; This is the "Graph Editor" sample program for the Piccolo2D structured 2D
; graphics framework. It draws 50 circular nodes, then draws 50 lines
; (called "edges" in the code) between randomly selected nodes. On mouseover,
; nodes turn red. As you drag a node to a new position, the lines stretch and
; stay with the node.
;
; You can find the original Java program at
;
; http://www.piccolo2d.org/learn/grapheditor.html
;
; (Click the "Java" hyperlink underneath the picture of the running program
; to get the Java source code used as the basis for this program.)
;
; ****************
; *IMPORTANT NOTE*  This program does not represent good Clojure style, so
; ****************
; please don't write Clojure programs that imitate this style. My purpose in
; writing this program was to discover how to use Clojure to do tasks commonly
; done in Java. YOU DO NOT NEED TO BE INTERESTED IN THE PICCOLO2D LIBRARY TO
; FIND SOMETHING OF INTEREST IN THIS CODE. It is not at all obvious how to use
; gen-class, how to subclass an existing Java class, how to mix Java constructs
; with Clojure data structures, or how to add event handlers to a Swing-based
; Clojure program. Studying the two versions of this program will enable you to
; creates certain kinds of Clojure programs much faster and with much less
; frustration than would otherwise be possible. Enjoy!
;
; -- Gregg Williams
;    (comments and feedback welcome: spamme AT-SIGN innerpaths PERIOD net)
;


 ; The gen-class below dictates that org.piccolo2d.PCanvas.GraphEditor
 ; be created as a full Java class. This can't be done using proxy because
 ; the GraphEditor class adds a new method, update-edge.
 (gen-class
   :extends org.piccolo2d.PCanvas
   :state state
   :init init
   :name  piccolotest.GraphEditor
   :post-init GraphEditorInit
   :constructors {[int int] []}
    )

  ;:methods [ [updateedge [org.piccolo2d.nodes.PPath] void] ]

(defn -init
  "Specifies arguments of class's constructor; returns
[ signature of superclass, value of state]."
  [width height]
  [[] { :num-nodes 50, :num-edges 50, :random (Random.) }])

(defn add-to-node
  "Adds edge to the Java array \"edges\" attached to node."
  [node edge]
  (let [current-count (.getAttribute node "num-used")
        ]
    (aset (.getAttribute node "edges") current-count edge)
    (println (str "edge " edge " added to " node " at position " current-count))
    (.addAttribute node "num-used" (inc current-count))))

(defn add-to-edge
  "Adds node to the Java array \"nodes\" attached to edge."
  [edge node]
  (let [current-count (.getAttribute edge "num-used")
        ]
    (aset (.getAttribute edge "nodes") current-count node)
    (println (str "node " node " added to " edge " at position " current-count))
    (.addAttribute edge "num-used" (inc current-count))))

(defn update-edge
      "Draws this edge, either initially or after endpoint node has been moved."
      [edge]
      (let [node1 (aget (.getAttribute edge "nodes") 0)
            node2 (aget (.getAttribute edge "nodes") 1)
            start (.. node1 getFullBoundsReference getCenter2D)
            end   (.. node2 getFullBoundsReference getCenter2D)]
        (.reset edge)
        (println (str "updateedge: draw from (" (.getX start) " "  (.getY start)
                      ") to (" (.getX end) " "  (.getY end) ")" ))
        (.moveTo edge (.getX start) (.getY start))
        (.lineTo edge (.getX end) (.getY end))))

(defn -GraphEditorInit
  "Post-initialization function that runs after superclass constructor 
   has executed. \"this\" refers to the new GraphEditor that has been created."
  [this width height]
  (.setPreferredSize this (Dimension. width height))
  (let [ 
        {:keys [num-edges num-nodes random]} (.state this)
        node-layer (.getLayer this)
        edge-layer (PLayer.)
        node-vector   ; its value is on next line
        (loop [result [], x num-nodes]
          (if (zero? x)
            result   ; returned if true
            (recur (conj result (PPath/createEllipse
                                  (.nextInt random width)
                                  (.nextInt random height)
                                  20
                                  20))
              (dec x))))   ; returned if false
        num-nodes-per-edge   2
        seed-number 50
        ]

    (defn install-node [node]
      (.addChild node-layer node)
      (.addAttribute node "edges" (make-array PPath num-edges))
      (.addAttribute node "num-used" 0))

    (defn install-edge [edge]
      (.addChild edge-layer edge)
      (.addAttribute edge "nodes" (make-array PPath num-nodes-per-edge))
      (.addAttribute edge "num-used" 0))

    (.addChild (.getRoot this) edge-layer)
    (.addLayer (.getCamera this) 0 edge-layer)

    (println "vector")
    (println node-vector)
    (doall (for [nv node-vector]
             (install-node nv)))

    (defn random-from-num-nodes
      "Creates a vector of two random numbers in range 0 <= r < num-nodes.
Single argument is ignoredso that this fcn can be used by iterate."
      [_]
      (vector (.nextInt random num-nodes) (.nextInt random num-nodes)))

    (defn not-equal? [pair]
      (not= (nth pair 0) (nth pair 1)))

    (defn process-edge-connecting-nodes
      "Creates edge and connects it to two nodes given by indexes in pair."
      [pair]
      (let [n1 (nth pair 0)
            n2 (nth pair 1)
            edge  (org.piccolo2d.nodes.PPath/createLine 0 0 100 100)
            node1 (.getChild node-layer n1)
            node2 (.getChild node-layer n2)]
        (println (str  "Processing node " pair))
        (install-edge edge)
        (add-to-node node1 edge)
        (add-to-node node2 edge)
        (add-to-edge edge node1)
        (add-to-edge edge node2)
        (update-edge edge)
        (println "###### end process-edge-for-nodes ######")))

    ; YAY--finally, a dollop of functional code! The "drop 1" drops the
    ; seed-number from the front of the infinite sequence, leaving only
    ; vectors, each containing two random numbers, in sequence.
    ; Without "(doall ...)", edges don't get drawn.
    (let [random-pair-seq (drop 1 (iterate random-from-num-nodes seed-number))
          node-pairs (take num-edges (filter not-equal? random-pair-seq))]
      (doall (map process-edge-connecting-nodes node-pairs))
      (println "###### Done processing edges and nodes ######"))

    (println "\n\n###### DUMP OF NODES WITH THEIR EDGES ######")

    ; debugging code--dumps each node and the edges connected to it
    (doall (for [nv node-vector]
             (let [edges (.getAttribute nv "edges")
                   num-used-limit (.getAttribute nv "num-used")]
               (print "node" nv "\n")
               (loop [index 0]
                 (if (< index num-used-limit)
                   (do
                     (update-edge (aget edges index))
                     (recur (inc index)))))
               )))

    ; This is the Clojure code that implements the Java code starting with
    ;     nodeLayer.addInputEventListener(new PDragEventHandler()...
    ; It implements the necessary mouse event handlers.
    ;
    ; Chunks of functionality have been moved around to accommodate how Clojure
    ; does things. In particular, "proxy [PDragEventHandler]..." is the Clojure
    ; idiom needed to at event handlers to a new instance of PDragEventHandler.
    ; This is then given the name custom-handler so that an event filter can be
    ; added to it before it is attached to node-layer.
    ;
    ; A lot of non-obvious stuff is going on here. Study this code and the
    ; original Java code if you want to avoid a lot of trial-and-error.
    ;
    (let [filter (PInputEventFilter.)
          custom-handler   ; its value is on next line
          (proxy [PDragEventHandler] []
            (mouseEntered [e]
              (proxy-super mouseEntered e)
              (if (= (.getButton e) MouseEvent/NOBUTTON)
                (. (. e getPickedNode) setPaint Color/RED)))
            (mouseExited [e]
              (proxy-super mouseExited e)
              (if (= (.getButton e) MouseEvent/NOBUTTON)
                (. (. e getPickedNode) setPaint Color/WHITE)))
            (startDrag [e]
              (proxy-super startDrag e)
              (.setHandled e true)
              (. (. e getPickedNode) moveToFront))
            (drag [e]
              (proxy-super drag e)
              (let [node (.getPickedNode e)
                    edges (.getAttribute node "edges")
                    num-used-limit (.getAttribute node "num-used")]
                (loop [index 0]
                  (if (< index num-used-limit)
                    (do
                      (update-edge (aget edges index))
                      (recur (inc index))))))))]

      (.setOrMask filter (+ InputEvent/BUTTON1_MASK
                           InputEvent/BUTTON3_MASK))
      (.setEventFilter custom-handler filter)
      (.addInputEventListener node-layer custom-handler))))
