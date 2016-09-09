;;Simple namespace for managing
;;atom-based timers,typically stored
;;inside node metadata.  Ported from
;;quilsample.
(ns piccolotest.timers)

;;What we can do is use event handlers to synchronize with
;;our external, event-based clock.
(defprotocol IParametric
  (time-reference [obj]))

;;weird implementation.  Let's see if we can simplify it
;;in a bit.  There's probably a link with activities here...
(def timers (atom {}))

(defn add-timer
  ([timers time-atom duration nm on-time]
   (let [tnow   @time-atom
         tfinal (+ duration tnow)
         _      (swap! timers assoc nm time-atom)]
     (add-watch time-atom
                nm
                (fn [k r old new]                       
                  (do (when (>= new tfinal)
                        (do (remove-watch r k)
                            (swap! timers dissoc nm)
                            ))
                      (on-time new))))))
  ([time-atom duration nm on-time]
   (add-timer timers time-atom duration nm on-time)))

(defn drop-timer
  ([timers time-atom nm]
   (do (remove-watch time-atom nm)
       (swap! timers dissoc nm)))
  ([time-atom nm] (drop-timer timers time-atom nm))) 
