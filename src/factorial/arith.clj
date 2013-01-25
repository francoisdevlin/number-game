(ns factorial.arith)

(def primes
     [2 3 5 7 11 13 17 19 23 29 31 37])

(def prime-count (apply merge (map sorted-map primes (iterate (constantly 0) 0))))

(defn first-factor [num] 
  (first (filter (fn [x] (zero? (mod num x))) primes)))

(defn factorize [[num wip]]
  (let [f (first-factor num)]
    (if f
      [(/ num f) (update-in wip [f] inc)]
      [nil wip])))

(defn factor [num]
  (let [[x output] (first (drop-while 
          (fn [[x output]] (not (or (nil? x) (= 1 x)))) 
          (iterate factorize [num prime-count])))]
    (cond
      (nil? x) nil
      true (vec (vals output)))))

(defn to-num [seq]
  (reduce * (map (fn [e base] (Math/pow base e)) seq primes)))

(defn full-length
  [a]
  (take (count primes) (concat a (repeat (count primes) 0))))

(defn mult [a b]
  (map + (full-length a) (full-length b)))

(defn sqr [a]
  (mult a a))

(defn add [a b]
  (factor (int (+ (to-num a) (to-num b)))))

(defn sub [a b]
  (factor (int (- (to-num a) (to-num b)))))

