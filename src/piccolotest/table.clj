;;Experimental table implementation.
;;Child cells can be nodes, default
;;is to provide rectangles.
(ns piccolotest.table
  (:require [piccolotest.sample :refer :all])
  (:import  [org.piccolo2d PNode PLayer]
            [org.piccolo2d.nodes PPath]
            [org.piccolo2d.util PBounds]))

(defn ^PNode ->cell [row col w h]
  (doto ^PPath (->rect :white (* col w) (* row h) 
                       w h {:row row :col col})
        (.setStroke nil)))

;;this is a hack...
(defn add-cell-child [^PNode cl chld]
  (let [^PBounds bnds (get-bounds cl)
        x    (.getX bnds)
        y    (.getY bnds)
        h    (.getHeight bnds)]
    (do (add-child (node-parent cl) chld)
        (translate chld x (- y h h) #_(- h y))
        cl)))
        

;;It'd be nice to have a value set in the table, as well as a mapping of
;;value->color.  We can have multiple layers too, allowing for imagery.

;;a table is a layer of cells.
;;canvas is a panel, layers are groupings of shapes that confer
;;and respond to events with eachother.
(defn  ->table
  ([rows cols w h  cached?]
   (let [cell-data   (into []
                           (for [row (range rows)
                                 col (range cols)]
                             [[row col] (->cell row col w h)]))
         cells       (reduce (fn [acc [[row coll] c]]
                               (let [rw   (get acc row {})
                                     cols (assoc rw coll c)]
                                 (assoc acc row cols))) {} cell-data)
         ctor        (if cached? ->cache identity)]        
                                        ; background (->rect :white 0 0 (* w cols) (* h rows) {:cells cells})]
     (with-node-meta
       (ctor (->layer (map second cell-data)))
       {:cells cells
        :dimension [rows cols]}))))

(defn ->srm-table [units qtrs & {:keys [w h cached?] :or {w 60 h 20 cached? true}}]
  (->table units qtrs w h cached?))

(defn cells [t] (:cells (node-meta t)))
(defn get-cell [t row col] (get (get (cells t) row) col))
(defn clear-cells! [t]
  (reduce-kv (fn [acc _ xs]
               (reduce-kv 
                (fn [acc _ ^PNode nd]
                  (do (.removeAllChildren nd)
                      (set-paint! nd :white)
                      acc))
                acc
                xs))
             t
             (cells t)))
(defn dimension [t] (:dimension (node-meta t)))
(defn row-count [t] (first (dimension t)))
(defn col-count [t] (second (dimension t)))

(defn rows [t]
  (let [rws (cells t)]
    (for [r (range (count rows))]
      (vals (get rws r)))))

(defn row [t idx]
  (let [rws (cells t)]
    (vals (get rws idx))))

(defn cols [t]
  (let [rows (cells t)
        rs   (range (count rows))
        cls  (count (first (vals rows)))]
    (for [c (range cls)]
      (for [r  rs]
        (get (get rows r) c)))))

(defn col [t idx]
  (let [rs (cells t)] 
    (for [r  (range (count rs))]
      (get (get rs r) idx))))

(defn do-row [t idx f]
  (f (row t idx)))

(defn do-col [t idx f]
   (f (col t idx)))
        

(comment ;;testing
  (defn random-col-color! [t]
    (do-col t (rand-int (col-count t))
            #(doseq [c %] (set-paint! c (rand-nth [:red :yellow :green :blue])))))
  )
