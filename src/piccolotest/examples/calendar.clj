(ns piccolotest.examples.calendar)

;;var clickedNode = event.pickedNodes[0];
;;var globalTransform = clickedNode.getGlobalTransform();   ;;globalToLocal? 
;;var inverse = globalTransform.getInverse();  ;;localToGlobal?
;;camera.animateViewToTransform(inverse, 500)

;;$("#cameraScale").text("" + (clickedNode.displayScale || 1));

;;zoom-to-node (ala prezi)
;;We may just want to provide a transform stack..
;;So wherever we zoomed in from, we invert the zoom...
;;Maybe allow the backspace to revert to the previous coords.
(defn zoom-on-double-click [duration]
  (let [zoomtime (long duration)
        zoom-to  (fn zoom-to [cam ^PNode nd]
                   (.animateViewToTransform cam (global-transform  nd)
                                            zoomtime))
                   ]
    {:mouseClicked (fn [e]
                     (when (and (events/left-click? e)
                                (events/double-click? e))
                       (let [nd (events/picked-node e)]
                         (zoom-to (events/camera e) nd))))}))



;;This calendar example is a really cool demo and a useful
;;navigation idiom.

;;the display layer controls movement
;;when we click, we get a new candidate...
;;that candidate either moves us up or down into the
;;hierarchy somewhere..

;;If we click in an area that's not in the current
;;hierarchy, i.e. we can't go deeper, then we
;;back up.

;;This works really nicely with interactive
;;treemaps too.


;;The basic idiom here is that we mess with the
;;camera based on some foci.
;;In this case, there's a hierarchical relationship
;;with the foci.

;;ported from calendar example.
(defn types-test [xs x]
    (reduce (fn [acc tp]
              (if (instance? x tp)
                (reduced x)
                acc)) nil xs))

;;Types test is used with findup...
(defn find-up [nd test]
  (if (or (nil? nd) (test nd)) nd
      (find-up (node-parent nd) test)))

    ;; layer.addListener({
    ;;   click: function (event) {
    ;;     var newFocus;

    ;;     ;;get the new focus, which is whatever got picked.
    ;;     newFocus = event.pickedNodes[0];

    ;;     ;;compute the nextcandidate based on the pick.
    ;;     if (lastFocus == null || lastFocus instanceof PLayer) {
    ;;       ;;if we lost focus, or we're at the player, look for the
    ;;       ;;nearest containing month.                                                     
    ;;       newFocus = findUp(newFocus, typesTest([Month]));
    ;;     } else { ;look for a day or month node...
    ;;       newFocus = findUp(newFocus, typesTest([Day, Month]));
    ;;     }

    ;;     ;;if the next candidate is different than the current, zoom to it.
    ;;     if (lastFocus !== newFocus) {
    ;;       zoomTo(newFocus);
    ;;       return true;
    ;;     }
    ;;                            },
                       
    ;;   ;;zoom out on right clicks.
    ;;   mouseup: function (event) {
    ;;     if (event.event.button === 2) {
    ;;       zoomOut();
    ;;       return true;
    ;;     }
    ;;                              }

;;note: all the useful pan/zoom transforms for cameras
;;apply equally as well to arbitrary nodes...since the view bounds is                        
;;merely a box, we can envision the camera transformations as "covering" 
;;operations....so the language could be "cover" or "move-to-cover"
;;Thus, if I cover something with a camera or an arbitray object,                        
;;I'm performing an operation on its bounds...
(defn container-zoom [cam last-focus new-focus] 
  (let [new-focus (if (or (nil? last-focus) (instance? last-focus PLayer))
                    (find-up new-focus #(types-test % ['Month]))
                    (find-up new-focus #(types-test ['Day 'Month])))]
    (when-not (identical? last-focus new-focus)
      (zoom-to cam new-focus))))
                       
(defn zoom-hierarchically [base-layer find-next duration]
  (let [zoomtime (long duration)
        newFocus  (atom base-layer)
        lastFocus (atom @newFocus)
        
        zoom-to  (fn zoom-to [cam ^PNode nd]
                   (let [newf 
                   (.animateViewToTransform cam (global-transform  nd)
                                            zoomtime))
                   ]
    {:mouseClicked (fn [e]
                     (when (and (events/left-click? e)
                                (events/double-click? e))
                       (let [nd (events/picked-node e)]
                         (zoom-to (events/camera e) nd))))}))

   ;; function zoomTo(newFocus) {
   ;;    if (newFocus === null) {
   ;;      newFocus = layer;
   ;;    }
   ;;    lastFocus = newFocus;

   ;;    var globalTransform = newFocus.getGlobalTransform();
   ;;    var inverse = globalTransform.getInverse();
   ;;    var focusBounds = (newFocus === layer) ? layer.getFullBounds() : newFocus.getFullBounds();

   ;;    inverse.translate((camera.bounds.width - focusBounds.width) / 2, (camera.bounds.height - focusBounds.height) / 2);


   ;;    camera.animateViewToTransform(inverse, 500);
   ;;  }

   ;;  function zoomOut() {
   ;;    var newFocus = findUp(lastFocus.parent, typesTest([Day, Month, PLayer]));

   ;;    if (newFocus === null) {
   ;;      newFocus = layer;
   ;;    }


   ;;    zoomTo(newFocus);
   ;;  }

   ;;  var backButton = new PImage("http://allain.github.io/piccolo2d.js/examples/zoom-out.png");
   ;;  backButton.addListener({
   ;;    'click': function(event) {
   ;;      zoomOut();
   ;;      return true;
   ;;    }
   ;;  });



;;Note: they already have this implemented in piccolo extras..
;;we can probably just wrap that.  For now, all I want to do
;;is identify which node, and by proxy, entity is selected.
;;Selections should occur when we click on an entity.
;;From here, we can start building interactive property
;;views of the entity, perhaps using swing trees and the like.
(defn selector   [select deselect]
  {:mouseClicked nil
   :mouseExited  nil}
  )

