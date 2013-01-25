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

(count (last (take 2 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 3 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 4 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
(count (last (take 5 (iterate (& set (p apply concat) (p map brute-add)) #{[1]}))))
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

