(ns factorial.core
    (:use [factorial.graph]))

(def & comp)
(def p partial)

(answer 13 [2 3 5 7 12 144 143])
(answer 14 [2 3 5 7 12 144 143 35])
(prime-list 14 [2 3 5 7 12 144 143])
(answer 15 [2 3 5 7 12 144 143 35])
(answer 15 [2 3 5 7 12 144 143 35 36])
(count (answer 15 [2 3 5 7 12 144 143 36]))
(prime-list 16 [2 3 5 7 12 144 143])
(map (& count (p prime-list 15)) [
                                  [2 3 4 5 12 144 143 36 35]
                                  [2 3 5 7 12 144 143 35 36]
                                  [2 3 5 7 12 144 143 35 36]
                                 ])

(answer 16 [2 3 5 7 12 144 143 35 36])

(map (& count (p prime-list 16)) [
                                  [2 3 5 7 12 144 143 35]
                                  [2 3 5 7 12 144 143]
                                  [2 3 5 7 12 144 143 4]
                                  [2 3 5 7 12 144 143 10]
                                 ])

(answer 17 [2 3 5 7 12 144 143 35 17])

(map (& count (p prime-list 17)) [
                                 [2 3 5 7 12 144 143 35 17]
                                 [2 3 5 7 12 144 143 84 85]
                                 [2 3 4 5 12 144 143 36 35 17]
                                 ])
(prime-list 17 [2 3 5 7 12 144 143 17])

(answer 18 [2 3 5 7 12 144 143 35 17])
(map (& count (p prime-list 18)) [
                                 [2 3 5 7 12 144 143 35 17]
                                 [2 3 5 7 12 144 143 35 34]
                                 [2 3 5 7 12 144 143 84 85]
                                 ])
(prime-list 18 [2 3 5 7 12 144 143 17])


(defn do-it [n & entries]
  (let [da-fn (& count (p prime-list n))
        best (first (sort-by da-fn (remove (& zero? da-fn) entries)))
      ]
    (println (da-fn best))
    (println (str "1, " (answer n best)))
  ))

              
(do-it 19 
       [2 3 5 7 12 144 143 35 17 19]
       [2 3 5 7 12 144 143 35 36 9 324 323]
       [2 3 4 12 10 144 143 36 35 19 17]
       [2 3 4 12 10 144 143 36 35 19 17]
       [2 3 5 7 12 144 143 35 9 27 324 323]
       )

(do-it 20
       [2 3 5 7 12 144 143 35 17 19]
       )

(defn brute-add [seq]
  (for [x seq y seq op [+ - *]]
       (conj seq (op x y))))

(defn prep-mult-seq [coll]
  (let [ blacklist-nums (reduce (fn [accum [k v]]
                    (if (some #{k} coll)
                      (apply conj accum v)
                      accum))
                #{1 2} {
                12 [3]
                144 [12]
                11 [11]
                13 [13]
                17 [17]
                19 [19]
                23 [23]
                29 [29]
                31 [31]
                37 [37]
                143 [144 143]
                })
        ]
  (remove blacklist-nums coll)))

(defn brute-add-2 [seq]
  (concat
    (for [x seq y seq]
         (conj seq (+ x y)))
    (for [x seq y seq :when (> x y)]
         (conj seq (- x y)))
    (let [mult-seq (prep-mult-seq seq)]
      (for [x mult-seq y mult-seq]
           (conj seq (* x y)))
      )))

(defn duplicates? [coll]
  (not (= (count (distinct coll))
          (count coll))))

(def loser-numbers
     #{8 9 10 256 512 1024})

(def winners {
     6 #{12}
     7 #{144}
     8 #{143}
     9 #{35}
     })

(defn require-all-winners [n]
  (let [must-haves (reduce (& distinct concat) 
                           (map winners (range 1 (inc n))))]
    (fn [coll]
        (every? (set coll) must-haves))))

((require-all-winners 6) [1])

(defn sprawl [[n sets]]
  [(inc n) ((& 
                 set 
                 (p map (& first second))
                 (p group-by sort) 
                 (p remove duplicates?) 
                 (p remove (p some loser-numbers)) 
                 (p filter (require-all-winners n)) 
                 (p remove (p some neg?)) 
                 (p remove (p some zero?)) 
                 (p apply concat) 
                 (p map brute-add-2))
            sets)])

(defn level [n seed]
  (second (last (take n (iterate sprawl [2 seed]))))
  )

(level 10 #{[1]})
(count (level 8 #{[1]}))
(count (level 9 #{[1]}))
(count (level 10 #{[1]}))
(count (level 11 #{[1]}))
(take 20 (level 11 #{[1]}))

(apply do-it 13 (map rest (level 9 #{[1]})))
(apply do-it 14 (map rest (level 9 #{[1]})))
(apply do-it 15 (map rest (level 9 #{[1]})))
(apply do-it 16 (map rest (level 9 #{[1]})))
(apply do-it 17 (map rest (level 10 #{[1]})))
(apply do-it 18 (map rest (level 10 #{[1]})))
(apply do-it 19 (map rest (level 11 #{[1]})))
(apply do-it 14 (level 8 #{[1]}))
(apply do-it 13 (level 7 #{[1]}))

(count (last (take 9 (iterate sprawl #{[1]}))))
(last (take 8 (iterate sprawl #{[1]})))
(take 50 (last (take 8 (iterate sprawl #{[1]}))))

(count (last (take 5 (iterate (& set (p remove (p some zero?)) (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 6 (iterate (& set (p remove (p some zero?)) (p apply concat) (p map brute-add)) #{[1]}))))
       
(last (take 3 (iterate (& (p apply concat) (p map brute-add)) #{[1]})))
(count (last (take 5 (iterate (& (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 2 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 3 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 6 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 7 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 8 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(brute-add [1 2])
(brute-add [1 0])



(map (& count (p prime-list 20)) [
                                 [2 3 5 7 12 144 143 17 19]
                                 [2 3 5 7 12 144 143 35 34 38]
                                 [2 3 5 7 12 144 143 9 27 324 323]
                                 ])
(prime-list 20 [2 3 5 7 12 144 143 17 19])

(map (& count (p prime-list 21)) [
                                 [2 3 5 7 12 144 143 17 19]
                                 ;[2 3 5 7 12 144 143 35 34 38]
                                 ;[2 3 5 7 12 144 143 9 27 324 323]
                                 ])
(prime-list 21 [2 3 5 7 12 144 143 17 19])
(info 21 [2 3 5 7 12 144 143 17 19])

(map (& count (p prime-list 22)) [
                                 [2 3 5 7 12 144 143 13 17 19]
                                 [2 3 5 7 12 144 11 13 17 19]
                                 ])
(prime-list 22 [2 3 5 7 12 144 143 17 19 13])

(map (& count (p prime-list 23)) [
                                 [2 3 5 7 12 144 143 13 17 19 23]
                                 [2 3 5 7 12 144 143 13 17 19 23]
                                 ])

(prime-list 23 [2 3 5 7 12 144 11 13 17 19 23])

(map (& count (p prime-list 24)) [
                                 [2 3 5 7 12 144 143 13 17 19 23]
                                 ])

(prime-list 24 [2 3 5 7 12 144 11 13 17 19 23])

(map (& count (p prime-list 25)) [
                                 [2 3 5 7 12 144 143 13 17 19 23]
                                 [2 3 5 7 12 144 143 13 17 19 23 25]
                                 ])

(prime-list 25 [2 3 5 7 12 144 11 13 17 19 23])

