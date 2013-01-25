(ns factorial.core
    (:use [factorial.arith] [factorial.bin]))

;(def & comp)
;(def p partial)

(def some-non-zero?
     (p some (complement  zero?)))

(defn largest-bin 
  "This function is used as a worker in the breakdown fn.  It returns the largest power of 2 that remains in each prime factor, and also returns the remain binary seq for each element"
  [bin-seq]
  [(int (Math/pow 2 (dec (count bin-seq)))) (drop-while zero? (rest bin-seq))])

(defn breakdown-step [bin-seq]
  (reduce (fn [accum [pow bin-seq]]
              [
               (conj (first accum) pow)
               (conj (second accum) bin-seq)
               ]) [[] []] 
          (map largest-bin bin-seq)))

(defn breakdown [bin-seq]
  (take-while some-non-zero?
  (map first (rest (iterate (& breakdown-step second) [nil bin-seq])))))

(defn shift-left [int-seq]
  (map (fn [x] (int (/ x 2))) int-seq))

(def min-vectors
     (& (p take-while some-non-zero?)
        (p iterate shift-left)))


(defn info [factors]
  (let [goal factors
        bin-goal (bin-prep factors)
        break-vectors (breakdown bin-goal)
        meat-vectors (map (p map (fn [e] (if (#{1} e) 0 e))) (breakdown bin-goal))
        ugly-ones (map (p map (fn [e] (if (#{1} e) e 0))) (breakdown bin-goal))
        ]
    {:goal goal
    :bin-goal bin-goal
    :break-vectors break-vectors
    :meat-vectors meat-vectors
    :ugly-ones ugly-ones
    }
    ))

(def factors
     (apply merge (map (comp #(apply sorted-map %) (juxt identity factor)) (range 1 38))))

(def !37 (last (reductions mult (vals factors))))

(defn ! [n]
  (map (fn [prime] (int (/ n prime))) primes)
  )

(defn prime-powers [n prime]
  (take n (take-while (p > n) (iterate (p * prime) prime))))

(prime-powers 37 2)

primes 
(! 37)
(def start
     (map factor
     [2 3 5 7 12 144 143]))

(print factors)
(info !37)


(assoc (info !37) :start start)

!37
(bin-prep !37)
(breakdown (bin-prep !37))
(min-vectors (first (breakdown (bin-prep !37))))
(map min-vectors (breakdown (bin-prep !37)))
(set (apply concat (map min-vectors (breakdown (bin-prep !37)))))
(count (set (apply concat (map min-vectors (breakdown (bin-prep !37))))))

(bar (map (& foo reverse bin) !37))
(map foo (last (bar (map (& foo reverse bin) !37))))
(bar (map foo (last (bar (map (& foo reverse bin) !37)))))
(bos (map (& reverse bin) !37))
(bos (last (bos (bin-prep !37))))
(take 10 (breakdown (bin-prep !37)))

(map (& cost bin) !37)
(reduce + (map (& cost bin) !37))
(map (& println (p remove empty?) bin-prep) (reductions mult (vals factors)))
(map (& println (p map (& cost)) (p remove empty?) (p map (& bin))) (reductions mult (vals factors)))
;(pprint factors)


