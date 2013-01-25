(ns factorial.bin)

(def & comp)
(def p partial)

(defn bin-rep [[x result]]
  (if (zero? (mod x 2))
    [(/ x 2) (conj result 0)]
    [(/ (dec x) 2) (conj result 1)]))

(defn bin [x]
  (second 
    (first 
      (drop-while (fn [[n output]] (not (zero? n))) 
                  (iterate bin-rep [x []])))))

(defn cost [coll]
  (+ (dec (count coll)) (reduce + coll)))

(def bin-prep (p map (& reverse bin))) 

