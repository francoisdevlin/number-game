(ns factorial.graph
    (:use [factorial.arith] [factorial.bin]))

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

(def square-steps 
     (&
       reverse
       (p apply concat)
       (p map (juxt sqr identity ))
       rest 
       min-vectors ))
       
       (first (breakdown (bin-prep (! 37))))

(defn compare-seqs
  [a b]
  (compare
    (reduce + a)
    (reduce + b)))

(defn valid-seq
  [goal guess]
  (every? (p apply >=) (map vector goal guess)))

(defn next-step [knowns goal]
  (distinct
    (cond 
      ((set (map vec knowns)) (vec goal)) knowns
      true (concat knowns 
                   (last (partition-by (set knowns) (square-steps goal)))))))


(defn add-step [goal knowns]
  (distinct
    (cond 
      ((set (map vec knowns)) (vec goal)) knowns
      true (let [sorted (sort-by identity (& - compare-seqs) knowns)
                 biggest (first sorted)
                 next-item (first (drop-while (fn [e] (not (valid-seq goal e))) (map (fn[e] (mult biggest e)) (rest sorted))))]
             (conj (vec knowns) next-item)))))

(defn print-all [seq]
  (map (fn [e] (do (print e) e)) seq))

(defn info [n start-ints]
  (let [goal (! n)
        start (map factor start-ints)
        bin-goal (bin-prep goal)
        break-vectors (breakdown bin-goal)
        all-squares (reduce next-step start break-vectors)
        attempts (take 100 (iterate (p add-step goal) all-squares))
        ]
    {:goal goal
    :break-vectors break-vectors
    :attempts attempts
    :temp (first (drop-while (fn[e] (not (some #{goal} e))) attempts ))
    }
    ))

(defn prime-list [n start]
  (:temp (info  n start)))

(defn answer [n start]
  (apply str (interpose ", " (map (& str to-num) (prime-list n start)))))




