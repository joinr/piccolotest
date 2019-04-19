(ns piccolotest.examples.calendar
  (:require [piccolotest.sample :as picc :refer :all]
            [spork.cljgui.components.swing :as gui]
            [spork.util.datetime :as date]
            [piccolotest [events :as events]
             [properties :as props]
             [activities :as acts]]))

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
        zoom-to  (fn zoom-to [cam ^org.piccolo2d.PNode nd]
                   (animate-view-to-transform! cam nd zoomtime))
                   ]
    {:mouseClicked (fn [e]
                     (when (events/double-left-click? e)
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

  

;;note: all the useful pan/zoom transforms for cameras
;;apply equally as well to arbitrary nodes...since the view bounds is                        
;;merely a box, we can envision the camera transformations as "covering" 
;;operations....so the language could be "cover" or "move-to-cover"
;;Thus, if I cover something with a camera or an arbitray object,                        
;;I'm performing an operation on its bounds...
#_(defn container-zoom [cam last-focus new-focus] 
  (let [new-focus (if (or (nil? last-focus) (instance? last-focus PLayer))
                    (find-up new-focus #(types-test % ['Month]))
                    (find-up new-focus #(types-test ['Day 'Month])))]
    (when-not (identical? last-focus new-focus)
      (zoom-to cam new-focus))))

;;months contain days...
;;days contain?
(def month-width 600)
(def column-width (/ 600 7.0))

(def month-names
  ["January", "February", "March", "April", "May", "June",
   "July", "August", "September", "October", "November", "December"])
(def tasks ["Get Milk", "Phone Mom", "Call Work", "Run Tests", "Investigate Fridge Humming"])


(defn tag-node [tag nd]
  (with-node-meta nd {:type tag}))

(defn ->task-list [xs]
    (->>  xs
          (map (comp ->cartesian ->text))
          (apply ->spaced-stack 30.0)
          (tag-node :task)))

(defn ->day
  ([number tasks]
   (let [color (if number
                 (if (pos? (count tasks))
                   #_:white      [255 255 255]
                   #_:light-grey [235 235 235])
                 #_:dark-grey  [200 200 200])
         parent (-> (->layer)
                    (set-paint! color)
                    (set-bounds! [0 0 (* column-width 5) 500])
                    (add-child (->> (->text (str number))
                                    (->scale 5 5)
                                    (->translate 10 10)
                                    (->cartesian)))
                    )]        
     (if tasks
       (add-child parent
                  (->> (->task-list tasks)
                       (->translate 10 70 #_130)))
       parent)))
  ([number] (->day number nil)))

(def days-of-week
  ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"])

;;currently identical to original implementation.
(defn ->day-labels []
  (->> days-of-week
       (map   ->text)
       (map   ->cartesian)
       (map-indexed  #(->translate (* column-width %1) 50 %2))
       (->layer)
       #_(apply ->spaced-shelf column-width)))

    ;; var Month = PNode.subClass({
    ;;   init: function (year, month) {
    ;;     this._super({
    ;;       bounds: new PBounds(0, 0, monthWidth, 570),
    ;;       fillStyle: "rgb(200, 200, 200)"
    ;;     });

    ;;     this.addChild(new PText(Month.monthNames[month]).scale(2).translate(5, 5));

    ;;     var dayNames = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
    ;;     for (var dayIndex = 0; dayIndex < dayNames.length; dayIndex++) {
    ;;       this.addChild(new PText(dayNames[dayIndex]).translate(dayIndex * columnWidth, 50).scale(0.7));
    ;;     }

    ;;     this.firstDay = Date.build(year, month, 1);
    ;;     this.lastDay = Date.build(year, month + 1, 0);

    ;;     var daysInMonth = Math.ceil(this.lastDay.getDate() - this.firstDay.getDate() - 1);

    ;;     var columnNumber = Math.abs(this.firstDay.getDay());
    ;;     var currentY = 70;
    ;;     var currentDay = 0;


(defn day-stream [init-day]
  (let [week      (atom 0)
        get-week! (fn [dow]
                    (if (== dow 6)
                      (let [res @week
                            _  (swap! week inc)]
                        res)
                      @week))]
    (->> (range 7) ;dow-indices
         (cycle)   ;repeat
         (drop init-day) ;start at init-day
         (map-indexed
          (fn [dom dow]
            {:day-of-month dom
             :day-of-week dow
             :week  (get-week! dow)
             :label (days-of-week dow)})))))

(defn month->indexed-days [year month]
  (let [first-day     (date/simple-date year month 1)
        last-day      (date/simple-date year (inc month) 0)
        days-in-month (- (.getDate ^java.util.Date last-day)
                         (.getDate ^java.util.Date first-day))
        init-day      (.getDay ^java.util.Date first-day)];;0-6=>S-Sa
    (->> (day-stream init-day)
         (map #(assoc % :year year :month month))
         (take days-in-month))))

;;Note: using getDay is deprecated....should be going to
;;gregorian calendar.
(defn ->month
  [year month & {:keys [day->tasks]
                 :or {day->tasks (fn [d]
                                   (when (> (rand) 0.2)
                                     (repeatedly (max 1 (rand-int 5))
                                                 (fn [] (rand-nth tasks)))))}}]
  (let [color         [200 200 200]
        bounds        [0 0 month-width 570]
        first-day     (date/simple-date year month 1)
        last-day      (date/simple-date year (inc month) 0)
        days-in-month (- (.getDate ^java.util.Date last-day)
                         (.getDate ^java.util.Date first-day))
        month-node    (-> (->> (->text (get month-names (dec month)))
                               (->cartesian)
                               (->scale     2 2)
                               (->translate 5 5))
                          (set-bounds! bounds)
                          (set-paint!  color)
                          (add-child   (->day-labels)))
        init-y         70
        row-height     100
        row-scale      0.2
        get-xy       (fn [week dow]
                       [(* dow column-width)
                        (+ (* week row-height) init-y)])]
    (->> (for [{:keys [day-of-week day-of-month week label]}
               (month->indexed-days year month)]
           (let [[x y] (get-xy week day-of-week)]
             (->> (-> (->day day-of-month (day->tasks day-of-month))
                      (highlight-bounds! :black))
                  (tag-node :day)
                  (->scale row-scale row-scale)
                  (->translate x y))))
         (add-children month-node)
         (tag-node :month))))    



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

#_(defn ->zoom-to [base-layer new-focus old-focus]
  (fn zoom-to [cam ^PNode new-focus]
    (let [new-focus (or new-focus base-layer)
          _         (reset! lastFocus new-focus)
          global-xform (global-transform new-focus)
          inv          (inverse global-xform)
          cam-bounds   (get-bounds cam)
          focus-bounds (get-bounds new-focus)
          _ (.translate inv (/ (- (.width cam-bounds)  (.width focus-bounds)) 2.0)
                        (/ (- (.height cam-bounds) (.height focus-bounds)) 2.0))]
      (animate-view-to-transform! cam inv zoomtime))))

;;  function zoomOut() {
;;    var newFocus = findUp(lastFocus.parent, typesTest([Day, Month, PLayer]));

;;    if (newFocus === null) {
;;      newFocus = layer;
;;    }


;;    zoomTo(newFocus);
;;  }
#_(defn ->zoom-out [base-layer new-focus last-focus zoom-to]
  (fn zoom-out []
    (let [new-foc (or (find-up (.getParent @last-focus) (types-test [:day :month :layer]))
                      base-layer)
          _ (reset! new-focus new-foc)]
      (zoom-to new-foc))))

(def last-sample (atom nil))
;;could this be a node itself?  All it has to
;;do is install listeners on child...
(defn ->zoomer [base-layer zoomtime]
  (let [focus      (atom base-layer)
        last-focus (atom base-layer)
        zoom-to    (fn zoom-to [cam new-focus]
                     (let [new-focus    (or new-focus base-layer)
                           _            (reset! last-focus new-focus)
                           global-xform (global-transform new-focus)
                           inv          (inverse global-xform)
                           cam-bounds   (get-full-bounds cam)
                           focus-bounds (get-full-bounds new-focus)
                           _ (.translate inv
                                         (/ (- (.width  cam-bounds)  (.width focus-bounds))  2.0)
                                         (/ (- (.height cam-bounds)  (.height focus-bounds)) 2.0))
                           _ (println [:zooming-to (reset! last-sample
                                                           {:global global-xform :inv inv :cam cam-bounds :focus focus-bounds})])]
                       (if (identical? new-focus base-layer)
                         (center-on! cam new-focus zoomtime)
                         (animate-view-to-transform! cam global-xform #_inv zoomtime)
                         
                         )))
                      
        zoom-out  (fn zoom-out [cam]
                    (let [new-foc (or (find-up (node-parent @last-focus) (property-filter :type #{:month}))
                                      base-layer)
                          _ (println [:zooming-out (if (identical? new-foc base-layer) :base (node-meta new-foc))])
                          _ (reset! focus new-foc)]
                      (zoom-to cam new-foc)))]
    {:focus      focus
     :last-focus last-focus
     :zoom-to    zoom-to
     :zoom-out   zoom-out}))

;;  var backButton = new PImage("http://allain.github.io/piccolo2d.js/examples/zoom-out.png");
;;  backButton.addListener({
;;    'click': function(event) {
;;      zoomOut();
;;      return true;
;;    }
;;  });

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

(defn zoom-hierarchically [base-layer duration]
  (let [zoomtime  (long duration)
        zoomer    (->zoomer base-layer #_(property-filter :type #{:day :month}) duration)
        {:keys [zoom-to zoom-out last-focus focus]} zoomer]
    {:mouseClicked
     (fn [e]
       (cond (events/left-click? e)
             (let [nd (events/picked-node e)
                   _  (println [:left-click nd (node-meta nd)])
                   new-focus  (or (find-up nd (property-filter :type #{:day :month}))
                                  base-layer)
                   _ (println [:found new-focus (node-meta new-focus)])]
                 (when-not (identical? @last-focus new-focus)
                   (zoom-to (events/camera e) new-focus)))
               (events/right-click? e)  (zoom-out (events/camera e))
               :else true))}))

;;Note: they already have this implemented in piccolo extras..
;;we can probably just wrap that.  For now, all I want to do
;;is identify which node, and by proxy, entity is selected.
;;Selections should occur when we click on an entity.
;;From here, we can start building interactive property
;;views of the entity, perhaps using swing trees and the like.
(defn selector   [select deselect]
  {:mouseClicked nil
   :mouseExited  nil})


(defn render-cal []
  (let [months  (apply ->spaced-stack 180
                       (for [months (partition 4 (map inc (range 12)))]
                         (let [ms (mapv (fn [m] (->month 2017 m)) months)]
                           (-> (apply ->spaced-shelf 90 ms)
                               (add-child (->cartesian (->text "ShelfL")))
                               #_(highlight-bounds! :blue)))))
        zooming (zoom-hierarchically months 500)]
    (picc/render! months :handler (merge zooming (highlighter :blue)))))
