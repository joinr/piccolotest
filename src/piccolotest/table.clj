;;Experimental table implementation.
;;Child cells can be nodes, default
;;is to provide rectangles.
(ns piccolotest.table
  (:require [piccolotest.sample :refer :all])
  (:import  [org.piccolo2d PNode PLayer]
            [org.piccolo2d.nodes PPath]))

(defn ^PNode ->cell [row col w h]
  (doto ^PPath (->rect :white (* col w)
                       (* row h) w h {:row row :col col})
        (.setStroke nil)))

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
         cells         (reduce (fn [acc [[row coll] c]]
                                 (let [rw (get acc row {})
                                       cols (assoc rw coll c)]
                                   (assoc acc row cols))) {} cell-data)
         ctor (if cached? ->cache identity)]        
                                        ; background (->rect :white 0 0 (* w cols) (* h rows) {:cells cells})]
     (with-node-meta
       (ctor (->layer  (map second  cell-data)))
       {:cells cells}))))

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
