;;A ns for convenience wrappers
;;for property changes and the like.
(ns piccolotest.properties
  (:import [java.beans
            PropertyChangeListener
            PropertyChangeEvent]))

;;we have a couple of options here.
;;This will also work with arbitrary beans.
;;Allow the user to supply their own functions
;;for on-change, etc.
;;Often times, the event is enough.
;;Typically, the format goes..
;;propertyname, oldval, newval
;;So, we can hook into observers
;;pretty easily....

;;we can actually supply a property-map...
(defn property-listener [m]
  (reify java.beans.PropertyChangeListener
    (^void propertyChange [this ^PropertyChangeEvent e]
      (when-let [f (get m (.getPropertyName e))]
        (f (.getOldValue e) (.getNewValue e))))))

;      (.addPropertyChangeListener PCamera/PROPERTY_VIEW_TRANSFORM lis))))
