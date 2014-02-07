(ns best-hand.core
  (:gen-class))
  
(defn best-hand [hand]
  (letfn [(n-freq? [r n f] (= f ((frequencies (vals (frequencies r))) n)))
          (straight? [r] ((set (map set (partition 5 1 "A23456789TJQKA"))) (set r)))
          (flush? [s] (apply = s))]
    ((fn [[s r]] 
      (cond 
        (and (flush? s) (straight? r)) :straight-flush
        (n-freq? r 4 1) :four-of-a-kind
        (and (n-freq? r 2 1) (n-freq? r 3 1)) :full-house
        (flush? s) :flush
        (straight? r) :straight
        (n-freq? r 3 1) :three-of-a-kind
        (n-freq? r 2 2) :two-pair
        (n-freq? r 2 1) :pair
        :else :high-card))
    (apply map list hand))))
    
(defn -main
  [& args]
  (println "Your best hand is" (name (best-hand args))))
