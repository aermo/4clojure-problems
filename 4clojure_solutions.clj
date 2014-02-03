; 4clojure, #32
#_(println (reduce #(conj %1 %2 %2) [] [1 1 1 2 3 1 1 1 2]))


; 4clojure, #33
#_(println ((fn [l n] (mapcat #(repeat n %) l)) [1 2 3 1 1 2] 2))


; 4clojure, #39?
#_(println (mapcat #(list %1 %2) [:a :b :c] [1 1 1 2 3 1 1 1 2]))


; 4clojure, #40?
#_(println ((fn [s l] (rest (mapcat #(list s %) l))) 0 [1 1 1 2 3 1 1 1 2]))


; 4clojure, #41
#_(println ((fn [s n] (keep-indexed #(if (not= (mod (inc %1) n) 0) %2) s)) [1 2 3 4 5 6 7 8] 3))



; 4clojure, #42
#_(println (#(reduce * (range 1 (+ 1 %))) 5))


; 4clojure, #43
#_(println (#(apply map list (partition %2 %1)) [1 2 3 4 5 6 7 8 9 10 11 12] 4))

; 4clojure, #4?
#_(println (#(map first (partition-by identity %)) [1 1 1 2 3 1 1 1 2]))

; 4clojure, #53
#_(println ((fn f ([a] (f (first a) (rest a) [(list (first a))]))
                ([a1 a r] (if-let [b1 (first a)]
                            (if (< a1 b1) 
                              (f b1 (rest a) (cons (concat (first r) [b1]) (rest r)))
                              (f b1 (rest a) (cons [b1] r)))
                            (let [m (apply max (map count r))]
                              (if (< 1 m)
                                (last (keep #(if (= (count %) m) %) r))
                                []))))
) [5 6 3 4]))

#_(println ((comp #(if (< 1 (count %)) % [])
                #(map first 
                  (first (sort-by count >
                    (partition-by last 
                      (map vector % (reductions + (cons 0 (map 
                        (fn [[x1 x2]] (if (< x1 x2) 0 1)) (partition 2 1 %)))))))))
) [5 6 3 4 5]))

#_(comp #(if (<= 2 (count %)) 
          %
          '())
      #(map first (first 
        (sort-by (comp - count) 
                 (partition-by last 
                   (map vector % (reductions + (cons 0 (map 
                      (fn [[x1 x2]] (if (< x1 x2) 0 1)) (partition 2 1 %))))))))))

#_(fn [xs] (or (last (sort-by count (filter #(> (count %) 1)
  (reduce (fn [[wk wn] i]
    (if (or (= (count wk) 0) (> i (last wk)))
        [(conj wk i) wn]
        (if (and (> (count wk) (count wn)) (> (count wk) 1))
            [[i] wk]
            [[i] wn])
    ))
    [[] []]
    xs
)))) []))


; 4clojure, #54
#_(println ((fn [n s] 
        (for [x (range (int (/ (count s) n)))] (take n (nthnext s (* x n))))
) 3 (range 12)))


; 4clojure, #55
#_(println ((comp #(zipmap (map first %) (map count %)) #(partition-by identity (sort %))
) [1 2 3 3 2 1 1 1 1 1 2 2 5]))

; 4clojure, #56
#_(println (#(map last (sort-by first (map first (partition-by last (sort-by last (map-indexed vector %)))))
) [1 2 1 3 1 2 4]))

#_(fn [s]
  (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] s))
  
#_(
reduce
  (fn [a x] (if (some #(= x %) a) a (conj a x)))
  []
)


; 4clojure, #58
#_(println (((fn [& m] (fn [& s] (first (reduce (fn [r i] (list (apply i r)))) s (reverse m))))
 zero? #(mod % 8) +) 3 5 7 9))
; rest reverse) [1 2 3 4]))


; 4clojure, #59
#_(println (((fn [& f] (fn [& a] (map #(apply % a) f)))
+ max min) 2 3 5 1 6 4))


; 4clojure, #60
;(println (take 5 ((fn [f c1 & c2]
#_(println ((fn rds
  ([f i c]
      (cons i
        (lazy-seq 
          (when-first [c1 c]
            (rds f (f i c1) (rest c))))))
  ([f [i & c]] (rds f i c)))
;+ (range))))
conj [1] [2 3 4]))
; 4clojure, #60
;#_(println (take 5 ((fn [f c]
;  #_(let [[i c] (if c2 (vector c1 c2) ('() c1)))
;    ((fn foo [ii cc] (cons ii (lazy-seq (foo (first cc) (rest cc))))) (first c) (rest c))
;  
;  #_(reduce 
;    #(conj %1 (apply f (last %1) [%2])) 
;    [(first c1)]
;    (flatten (rest c1))))
;;conj [1 2 3 4]))
;+ (range))))


; 4clojure, #61
#_(println (#(apply merge (map (fn [a b] {a b}) %1 %2))
[:a :b :c] [1 2 3]))

#_(println (#(apply assoc {} (interleave %1 %2))
[:a :b :c] [1 2 3]))


; 4clojure, #62
#_(println (take 5 ((fn itr [f i] (cons i (lazy-seq (itr f (f i)))))
#(* 2 %) 1)))


; 4clojure, #63
#_(println ((fn [f i] (reduce #(assoc %1 (f (first %2)) %2) {} (partition-by f (sort-by f i))))
#(apply / %) [[1 2] [2 4] [4 6] [3 6]]))

#_(println ((fn [f s]
  (reduce (fn [m a]
    (let [x (f a)]
      (assoc m x (conj (get m x []) a)))) {} s))
#(apply / %) [[1 2] [2 4] [4 6] [3 6]]))


; 4clojure, #65
;(println (map (
#_(println (
  #(case (empty %)
              #{} :set
              {} :map
              [] (if (first (conj (conj (empty %) false) true)) :list :vector))
;{:a 1, :b 2}))
;(range (rand-int 20))))
;[1 2 3 4 5 6]))
#{10 (rand-int 5)}))
;[{} #{} [] ()])))

; 4clojure, #66
#_(fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))

;#(if (zero? %2) %1 (recur %2 (mod %1 %2)))


; 4clojure, #67
#_(println ((fn [n]
  ((fn pchk [cand c] 
    (if (some #(= 0 %) (map #(mod cand %) c))
      (pchk (inc cand) c) 
      (if (< (count c) (dec n))
        (pchk (inc cand) (conj c cand))
        (conj c cand))))
  3 [2]))
5))

#_(println ((fn [n]
  (take n
    (filter
      (fn [cand] (every? pos? (map #(mod cand %) (range 2 cand))))
      (iterate inc 2))))
5))


; 4clojure, #69
#_(println ((fn [m & c] (reduce (fn [a b] 
                            (reduce (fn [ai [k v]]
                                (if (contains? ai k)
                                  (update-in ai [k] #(apply m [% v]))
                                  (merge ai {k v})))
                              a b))
                          c))
;* {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}
;- {1 10, 2 20} {1 3, 2 10, 3 15}
concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}
))

#_(println ((fn [m & c] (reduce (fn [a b] 
                            (reduce (fn [ai [k v]]
                                  (assoc ai k (if-let [vi (ai k)] (m vi v) v)))
                              a b))
                          c))
;* {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}
;- {1 10, 2 20} {1 3, 2 10, 3 15}
concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}
))

; 4clojure, #70
#_(println (#(sort-by clojure.string/lower-case (clojure.string/split % #"\W"))
"Have a nice day."))


; 4clojure, #73
#_(println
  ((fn [brd]
    ((fn [directions]
      (->>
        (map #(if (apply = %) (first %)) directions)
        (filter #(or (= :o %) (= :x %)))
        (first)
      ))
  (concat
   brd ;rows
   (partition 3 (apply interleave brd)) ;cols
   [(map #(nth (nth brd %) %) (range 3))] ;cross1
   [(map #(nth (nth (reverse brd) %) %) (range 3))]))) ;cross2
[[:o :e :o] [:x :o :e] [:x :e :o]]))

; This works with a N x N board for arbitrary N.
#_(fn [b]
  (let [rows #(apply (partial map vector) %)
        nn   #(nthnext % %2)
        won? (fn [p]
               (some #(apply = (conj % p))
                 (concat
                   b (rows b)
                   (rows (map nn b (range)))
                   (rows (map nn (reverse b) (range))))))]
    (cond (won? :x) :x
          (won? :o) :o
          :else     nil)))
(println ((fn [brd]
  (let [transpose #(partition (count %) (apply interleave %))
        diag #(transpose (map (fn [r c] [(nth r c)]) % (range)))
        data (concat brd (transpose brd) (diag brd) (diag (reverse brd)))
        won? (fn [p] (some #(apply = (conj % p)) data))]
    (cond
      (won? :x) :x 
      (won? :o) :o
      :else nil)))
[[:e :x :e] [:o :o :o] [:x :e :x]]))


(println ((fn [brd]
  (let [diag #(mapcat (fn [r c] [(nth r c)]) % (range))
        data (concat brd (map vector brd) [(diag brd) (diag (reverse brd))])
        won? (fn [p] (some #(apply = (conj % p)) data))]
    (cond
      (won? :x) :x 
      (won? :o) :o
      :else nil)))
[[:e :x :e] [:o :o :o] [:x :e :x]]))

; 4clojure, #74
#_(println (
  (fn [s]
    (let [c (map read-string (clojure.string/split s #"\D"))
          q (take-while #(<= % (apply max c))
              ((fn f [n] (cons (* n n) (lazy-seq (f (inc n))))) 1))]
    (clojure.string/join "," (filter (fn [i] (some #(= % i) q)) c))))
"15,16,25,36,37"))

#_(println (
(fn [s]
  (->> s
    (re-seq #"\d+")
    (map read-string)
    (filter #(let [r (int (Math/sqrt %))] (= % (* r r))))
    (interpose ",")
    (apply str)))
"4,5,6,7,8,9"))

; 4clojure, #75
#_(println (
(fn [n] 
  (count 
   (filter 
    (fn [i] (= (#(if (zero? %2) %1 (recur %2 (mod %1 %2))) i n) 1))
    (range 1 (inc n)))))
40))


; 4clojure, #76
#_(println (
(letfn [(foo [x y] #(bar (conj x y) y)) (bar [x y] (if (> (last x) 10) x #(foo x (+ 2 y))))] (trampoline foo [] 1))
))


; 4clojure, #77
#_(fn [v]
  (->>
    (group-by #(sort %) v)
    (vals)
    (filter #(< 1 (count %)))
    (map set)
    (set)
  ))
;["meat" "mat" "team" "mate" "eat"]




; 4clojure, #78
#_(defn myfun [f & args]
  (loop [loopf (apply f args)]
    (if (fn? loopf)
      (recur (loopf))
      loopf)))
;testing 
;letfn [(triple [x] #(sub-two (* 3 x)))
;       (sub-two [x] #(stop?(- x 2)))
;       (stop? [x] (if (> x 50) x #(triple x)))]
;(myfun triple 2)



;4 clojure, #79
#_(fn [g] 
  (apply min
  (reduce 
    (fn [a b] 
        (map + 
          (map #(apply min %)
            (partition 2 1 (concat [(first a)] a [(last a)])))
          b))
    g)))
; '([1]
;  [2 4]
; [5 1 4]
;[2 3 4 5])
;   '([3]
;    [2 4]
;   [1 9 3]
;  [9 9 2 4]
; [4 6 6 7 8]
;[5 7 3 5 1 4])
;   '([3]
;    [2 4]
;   [1 9 3]
;  [9 9 2 4])


; 4clojure, #80
#_((fn [n] (= (apply + (filter #(= 0 (mod n %)) (range 1 n))) n))
496)

#_(def dropletter (fn [word ind] (apply str (concat (take ind word) (drop (inc ind) word)))))
#_(def subchk (fn [w1 w2] (some true? (map #(= (dropletter w1 %) (dropletter w2 %)) (range (count w2))))))
#_(def inschk (fn [w1 w2] (some true? (map #(= w1 (dropletter w2 %)) (range (count w2))))))
#_(def delchk #(inschk %2 %1))
#_(def doChecks (fn [w1 w2] (some true? (map #(% w1 w2) [subchk inschk delchk]))))
#_(def iterCheck (fn [w s]
              (if (empty? s)
                true ; <--------- No more words to check -> success.
                (some true? ; <-- Only one successful path required.
                  (map ; <------- Branch check paths with mapper.
                    (fn [sn] 
                      (if (doChecks w sn)
                        (iterCheck sn (remove #(= % sn) s)) ; Branch here.
                        false)) ; No success with this path.
                    s)))))
; 4clojure, #82
#_(fn [wset]
  (letfn [(dropletter [word ind] (apply str (concat (take ind word) (drop (inc ind) word))))
          (subchk [w1 w2] (some true? (map #(= (dropletter w1 %) (dropletter w2 %)) (range (count w2)))))
          (inschk [w1 w2] (some true? (map #(= w1 (dropletter w2 %)) (range (count w2)))))
          (delchk [w1 w2] (inschk w2 w1))
          (doChecks [w1 w2] (some true? (map #(% w1 w2) [subchk inschk delchk])))
          (iterCheck [w s]
              (if (empty? s)
                true ; <--------- No more words to check -> success.
                (some true? ; <-- Only one successful path required.
                  (map ; <------- Branch check paths with mapper.
                    (fn [sn] 
                      (if (doChecks w sn)
                        (iterCheck sn (remove #(= % sn) s)) ; Branch here.
                        false)) ; No success with this path.
                    s))))]
    (boolean 
      (some true? 
        (map 
          (fn [w] (iterCheck w (remove #(= % w) wset)))
          wset)))))

#_(fn [wset]
  (letfn [(dropletter [word]
            (map #(apply str (concat (take % word) (drop (inc %) word))) (range (count word))))
          (doChecks [w1 w2] 
            (let [w1s (dropletter w1) w2s (dropletter w2)] 
              (some true? [(some #(= % w2) w1s)
                           (some #(= % w1) w2s)
                           (some true? (map = w1s w2s))])))
          (iterCheck [w s]
              (if (empty? s)
                true ; <--------- No more words to check -> success.
                (some ; <-------- Only one successful path required.
                  (fn [sn] 
                    (if (doChecks w sn)
                      (iterCheck sn (remove #(= % sn) s)) ; Branch here.
                      false)) ; No success with this path.
                s)))]
    (boolean (some (fn [w] (iterCheck w (remove #(= % w) wset))) wset))))
;#{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"} ;true
;#{"cot" "hot" "bat" "fat"} ;false
;#{"spout" "do" "pot" "pout" "spot" "dot"} ;true

; 4clojure, #84
#_(fn [rels] (loop [rs rels]
  (let [newrs (into rs
                (for [[r11 r12] rs [r21 r22] rs :when (= r11 r22)]
                  [r21 r12]))]
    (if (= newrs rs) rs (recur newrs)))))
;#{[8 4] [9 3] [4 2] [27 9]}


; 4clojure, #85
#_(fn [items]
  (loop [s #{items} r #{items}]
    (let [t (reduce (fn [p j] (into p (map (fn [i] (set (remove #(= i %) j))) j))) #{} s)]
    (if (= (count (first t)) 0)
      (into r t)
      (recur t (into r t))))))
;#{1 2 3}

#_(fn [items] 
  (reduce
    (fn [s i] 
      (into s (map #(conj % i) s)))
    #{#{}} items))
;#{1 2 3}


; 4clojure, #86
#_(fn [n]
    (loop [n n s #{}]
      (let [r (conj s n)]
      (if (= s r)
        (if (= n 1) true false)
        (recur (apply + (map (comp #(* % %) int read-string str) (str n))) r)))))
;2

; 4clojure, #89
#_(fn [g]
  (let [s (map conj g (range)) ; conjoin ids to edges
        check (fn iterCheck [n s]
              (if (empty? s)
                true
                (some
                  (fn [e] 
                   (if (some #(= % n) (take 2 e))
                     (iterCheck (first (remove #(= n %) e)) (remove #(= e %) s))
                     false))
                 s)))]
    (boolean (some (fn [e] (check (first e) (remove #(= e %) s))) s))))
;[[1 2] [2 3] [3 4] [4 1]]
;[[:a :c] [:c :b] [:a :e] [:a :d] [:b :d] [:c :e] [:d :e] [:c :f] [:d :f] [:a :b] [:b :e]]
;[[1 2] [2 3] [2 4] [2 5]]


; 4clojure, #90
#_(fn [a b] (set (for [c a d b] [c d])))
;#{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"}


; 4clojure, #91
#_(fn [s]
  (letfn [(check [c s] ; (c: connected nodes, s: candidate edges)
            (if (empty? s)
              true
              ; filter edges which may be connected
              (let [e (filter (fn [m] (some (fn [n] (some #(= % n) c)) m)) s)]
                (if (empty? e)
                  false
                  (recur (reduce into c e) (filter (fn [m] (not (some #(= % m) e))) s))))))]
    (check (set (first s)) (rest s))))
;#{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [3 4]}
;#{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e]}
;#{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]}


; 4clojure, #92
#_(fn [s] (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
               v (vec (map #(m %) s))]
          (->>
            (range (dec (count v)))
            (map #(if (< (v %) (v (inc %))) (- (v %)) (v %)))
            (apply +)
            (+ (last v))
          )))
;"XIV"
;"DCCCXXVII"
;"MMMCMXCIX"


; 4clojure, #93
#_(fn [c]
  (letfn [(f [c] 
    (if (coll? (first c))
      (reduce #(apply conj %1 (f %2)) [] c)
      [c]))]
  (let [r (into (empty c) (f c))]
    (if (list? r) (reverse r) r))))
;[[[[:a :b]]] [[:c :d]] [:e :f]]
;'((1 2)((3 4)((((5 6))))))


#_(fn f [c]
  (mapcat
    #(if (coll? (first %))
         (f %)
         (list %))
    c))
;[[[[:a :b]]] [[:c :d]] [:e :f]]




; 4clojure, #93
#_(fn [b]
  (let [c (count b)
        b (map (fn [l] (map #(if (= \# %) 1 0) l)) b)
        trps #(apply map vector %)
        rowc #(map
                (fn [l]
                  (map
                    (fn [n]
                      (apply + (take (if (zero? n) 2 3) (drop (dec n) (nth % l)))))
                    (range c)))
                (range c))]
  (map 
    (fn [lb lp] 
      (apply str
        (map 
          (fn [nb np]
            (if (zero? nb)
              (if (= np 3) "#" " ")
              (if (or (= np 3) (= np 4)) "#" " ")))
          lb
          lp)))
    b
    ((comp trps rowc trps rowc) b))))
;["      "  
; " ##   "
; " ##   "
; "   ## "
; "   ## "
; "      "]


; 4clojure, #95
#_(fn f [t]
  (if (coll? t)
    (if (= (count t) 3))
      (every? f t)
      false)
    (or (nil? t) t))


; 4clojure, #96
#_(fn [[p l r]]
  (= l
     ((fn f [[p l r :as t]]
        (if (coll? t)
          [p (f r) (f l)]
          t))
      r)))
;'(:a (:b nil nil) (:b nil nil))
;[1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]] [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]
;'(:a (:b nil nil) (:c nil nil))



; 4clojure, #97
#_(fn [n]
  (loop [r [1] i 1]
    (if (< i n)
      (let [r (cons 0 r)]
        (recur (map #(apply + (take 2 (drop % r))) (range (count r))) (inc i)))
      r)))
;11

#_(fn [n]
  (loop [r [1] i 1]
    (if (< i n)
      (recur 
        (map #(apply + %) (partition-all 2 1 (cons 0 r)))
        (inc i))
      r)))
;11


; 4clojure, #98
#_(fn [f s]
  (->>
    (group-by f s)
    (vals)
    (map set)
    (set)))
;#(* % %) #{-2 -1 0 1 2}

; 4clojure, #99
#_#(map (comp read-string str) (str (* %1 %2)))


; 4clojure, #99
#_(fn [& args] 
  (/
    (apply * args)
    (reduce #(if (zero? %2) %1 (recur %2 (mod %1 %2))) args)))
;3/4 1/6


; 4clojure, #101 # Levenshtein Distance ########################
;  Difficulty:	Hard
;  Topics:	seqs
#_(fn [a b]
  (let [[a b] (sort-by count [a b])
        rm-one (fn [w] ; removes one subitem from each item
                  (map 
                   #(concat (take % w) (drop (inc %) w))
                   (range (count w))))]
    (loop [a (vec a) b [(vec b)] n 0]
      (if (< (count a) (count (first b)))
        (recur a (set (mapcat rm-one b)) (inc n))
        (+ n (apply min 
                  (map
                    (fn [bi] (apply + (map #(if (= %1 %2) 0 1) a bi)))
                    b)))))))
                

;"kitten" "sitting"
;"clojure" "closure"
;"asitting" "bsittinga"
;"ttttattttctg" "tcaaccctaccat"



;#_(fn [a b]
;  (letfn [(rm-one [w] ; remove one subitem from each item
;            (set
;              (mapcat
;                (fn [i]
;                  (map 
;                   #(concat (take % i) (drop (inc %) i))
;                   (range (count i)))))
;                 w)
;           (seln [w n] ; select n subitems from each item
;            (if (< n (count (first w)))
;              (if (< (inc n) (count (first w)))
;                (set (mapcat #(seln % n) (rm-one w)))
;                (map #(seln % n) (rm-one w)))
;              w))
;           (iter-check [a b k]
;            (if (some = a b)
;              k
;              (let [ca (count (first a))
;                    cb (count (first b))
;                    [c d n] (if (= ca cb)
;                              [(rm-one a) (rm-one b) (+ k 1)]
;                              (if (< ca cb)
;                                  [[a] (seln b ca) (- cb ca)]
;                                  [(seln a cb) [b] (- ca cb)]))
;;                    pm (map-indexed 
;;                        (fn [i n] 
;;                          (map-indexed 
;;                            #((vector i %1 * (compare %2 n) (compare %2 n)))
;;                            d))
;;                          c)
;;                    minm (apply min-key #(map last %) (map (fn [i] (apply min-key #(map last %) i)) pm))
;                    ]
;                (+ n (apply min (map (fn [i] (apply min (map #(iter-check i %) c)) d)))))))]
;    (iter-check [(vec a)] [(vec b)] 0))))
;"kitten" "sitting"
;"clojure" "closure"

; 4clojure, #102
#_(fn [s] (let [s (clojure.string/split s #"[-]")]
  (clojure.string/join 
    (cons
      (first s)
      (map #(clojure.string/capitalize %) (rest s))))))
;"multi-word-key"





#_(fn [n a]
  (letfn [(rm-one [w]
            (map 
               #(set (concat (take % i) (drop (inc %) i)))
               (range (count i))))]
    (loop [n (- (count a) n) a #{a}]
      (if (< 0 n)
        (recur (dec n) (set (mapcat rm-one a)))
        (if (> 0 n) 
          #{}
          a)))))
          

;(reduce (reduce (conj )))
;(filter #(= 2 (count %))
;1 #{4 5 6}



; 4clojure, #104
#_(fn [x]
  (let [m [[1000 "M"] [900 "CM"] [500 "D"] [400 "DM"] [100 "C"]
           [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
           [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
    (loop [x x s ""]
      (if (< x 1)
        s
        (let [i (first (filter #(<= (first %) x) m))]
          (recur (- x (first i)) (str s (last i))))))))
;140


; 4clojure, #105
#_(fn [v]
  (first (reduce 
    #(if (keyword? %2)
      [(merge (last %1) {%2 (vec (butlast %1))})]
      (cons %2 %1))
    [{}] (reverse v))))

#_(fn [v]
  (->> (partition-by keyword? v)
    (mapcat
      (fn [[k :as x]]
        (if (keyword? k) (interpose [] x) [x])))
    (apply array-map)))
;[]
;[:a 1 2 3 :b :c 4]
  


;4clojure, #106 # Number Maze ###############################
;  Difficulty:	Hard
;  Topics:	numbers
#_(fn [a b]
  (letfn [(ops [n] (list (if (even? n) (/ n 2) n) (* n 2) (+ n 2)))]
  (loop [a #{a} n 1]
    (if (some #(= % b) a)
      n
      (recur (set (mapcat ops a)) (inc n))))))
;3 8


#_(fn [& a]
  (first
    (drop-while 
      (fn [i] 
        (some 
          (fn [v] (not= i (first (drop-while  #(< % i) v))))
          (rest a)))
      (first a))))
;[1 2 3 4 5 6 7] [0.5 3/2 4 19]
;(range) (range 0 100 7/6) [2 3 5 7 11 13]
;(map #(* % % %) (range)) ;; perfect cubes
;(filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
;(iterate inc 20) ;; at least as large as 20

;4clojure, #110

;(fn [s]
;  (rest
;    (iterate
;      #(mapcat
;        (juxt count first)
;        (partition-by identity %))
;     s)))
;(take 5 (f [1])


#_(fn [w b] 
  (letfn [(clean [b] (map #(re-seq #"[\w#]" %) b))
          (split-at-# [w] (partition-by #(= "#" %) w))
          (wordcmp [pfw]
              (every? true? (map 
                              #(if (some #{%2} ["_" %1]) true false)
                              (re-seq #"\w" w) pfw)))]
    (->>
      (apply map list (clean b)) ; cols
      (concat (clean b)) ; rows
      (mapcat split-at-#)
      (filter #(= (count w) (count %)))
      (some #(wordcmp %))
      (boolean)
    )))
;"clojure" ["_ _ _ # j o y"
;           "_ _ o _ _ _ _"
;           "_ _ f _ # _ _"]


#_(fn f [a s]
  (into []
    (when-first [fs s]
      (let [i (if (coll? fs) (f a fs) fs)
            a (- a (apply + (flatten [i])))]
      (if (<= 0 a)
        (cons i (lazy-seq (f a (rest s)))))))))

;10 [1 2 [3 [4 5] 6] 7]
;30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11]
;9 (range)
;0 [1 2 [3 [4 5] 6] 7]


; 4clojure, #113
;seq (apply
;str (apply
;#(reify
;  clojure.lang.ISeq
;  (seq [_] (distinct %&))
;  (toString [_] (apply str (interpose ", " (sort %&)))))
;(repeat 5 1))

;#(reify clojure.lang.Seqable
;    (toString [_] (apply str (interpose ", " (sort %&))))
;    (seq [_] (seq (distinct %&))))



;4clojure, #114
#_(fn f [n p s]
  (when-first [fs s]
    (let [n (if (p fs) (dec n) n)]
      (if (< 0 n)
        (cons fs (f n p (rest s)))))))
;400 #(= 2 (mod % 3))
;         [2 3 5 7 11 13 17 19 23]
  
  
    
; #115
#_(fn [n]
  (let [s (map int (str n)) cs (count s)]
    (= (apply + (take (/ cs 2) s))
       (apply + (drop (/ (dec cs) 2) s)))))
;120121


; #116 Prime Sandwich
;  Difficulty:	Medium
;  Topics:	math
;take 16 (filter
#_(fn [n]
  (if (< n 4) false
    (let [f (fn f [[fs & s]]
              (cons fs
                    (lazy-seq
                      (f (filter
                          #(not= 0 (mod % fs))
                          s)))))
         r (last (take-while #(<= (second %) n) (partition 3 1 (f (iterate inc 2)))))]
       (and (= n (second r)) (= (/ (+ (first r) (last r)) 2) n)))))
  ;     np (first (filter #(every? pos? (map (partial mod %) ps)) (iterate inc (last ps))))]
     ;(println (drop (- (count p1) 2) p1) (drop (- (count p2) 2) p2))
  ;   (and (= (last ps) n) (= (apply - (take-last 2 ps)) (- (last ps) np))))))

#_(fn [n]
  (loop [s [2] i 3]
    (if (every? pos? (map (partial mod i) s))
      (if (<= n (first s))
        (let [r (take 3 (cons i s))]
          (and (= 3 (count r)) 
               (= n (second r))
               (= (apply - (take 2 r)) (apply - (drop 1 r)))))
        (recur (cons i s) (inc i)))
      (recur s (inc i)))))
;563
;(range))
;0


; #117 # For Science! ##########################################
;  Difficulty:	Hard
;  Topics:	game
#_(fn [m]
  (let [m (map #(str "#" % "#") m)
        w (count (first m))
        m (concat [(apply str (repeat w "#"))] m [(apply str (repeat w "#"))])
        h (count m)
        chk (fn [m c] (reduce 
                        (fn [s r] (+ s (count (filter #(= c %) r))))
                        0 m))
        move (fn [s] (if (some #(= \M %) s)
                        (apply str (repeat (count s) \M))
                        s))
        step (fn [m] (map 
                        (fn [r]
                          (->>
                            (interleave
                              (cons "_" (map move (re-seq #"[\w\s]+" r)))
                              (re-seq #"#+" r))
                            (rest)
                            (apply str)
                          ))
                        m))]
    (loop [m m n 0]
      (if (< (chk m \C) 1)
        true
        (if (= (chk m \M) n)
          false
          (recur (apply map str (step (apply map str (step m))))
                (chk m \M)))))))
;["M  # C"]

;["#######"
;"#     #"
;"#  #  #"
;"#M # C#"
;"#######"]



; #119 # Win at Tic-Tac-Toe ##################################
;  Difficulty:	Hard
;  Topics:	game
#_(fn [p brd]
  (let [brd (map
              (fn [rb r] (map (fn [ib c] [ib r c]) rb (range)))
              brd (range))
        diag #(mapcat (fn [r c] [(nth r c)]) % (range))
        data (concat brd 
                    (apply map vector brd)
                    [(diag brd) (diag (reverse brd))])]
    (reduce
      (fn [s r]
        (if (= 2 (count (filter #(= (first %) p) r)))
          (reduce 
            #(if (= :e (first %2)) (conj %1 (vec (rest %2))) %1)
            s r)
          s))
      #{} data)))
;:x [[:e :x :e] [:o :x :o] [:x :e :x]]


; #120 ###########################################
#_(fn [s]
  (count 
    (filter 
      #(->>
        (re-seq #"\d" (str %))
        (map read-string)
        (map (fn [i] (* i i)))
        (apply +)
        (< %)
      )
    s)))
;(range 10)

; #121 #########################################


;(
;(fn [[f & args]]
;  (fn [m]
;    (apply ({'+ + '- - '* * '/ /} f)
;           (map #(if (symbol? %) (m %) %) args))))
;'(/ a b)) '{b 8 a 16}

#_(
(fn [f]
 (fn [m]
  ((fn r [a]
     (if (seq? a)
       (apply ({'- - '* * '/ /} (first a) +) 
              (map r (rest a)))
       (m a a)))
   f)))
'(/ a b))
;'{b 8 a 16}


;#124 # Analyze Reversi #################################################
; Difficulty:	Hard
; Topics:	game 

#_(fn [brd plr]
  (let [opp ('{w b b w} plr)
        drct (for [i [-1 0 1] j [-1 0 1]] [i j])
        chk-drct (fn [i d s]
                  (if (= 'e (get-in brd i))
                    (loop [i (map + i d) o #{}]
                      (let [p (get-in brd i)]
                        (cond
                          (= p opp) (recur (map + i d) (conj o i))
                          (= p plr) (into s o)
                          :else  s )))))]
    (into {}
      (remove #(empty? (last %))
        (for [r (range (count brd)) c (range (count (first brd)))]
                (vector [r c] (reduce #(chk-drct [r c] %2 %1) #{} drct)))))))
; '[[e e e e] [e w b e] [e b w e] [e e e e]] 'w



;#125 # Fail ############
#_(fn []
  (let [a (str '(fn [] (let [a (str a)] (apply str (concat (take 22 a) a (drop 23 a))))))]
  (apply str (concat (take 22 a) a (drop 23 a)))))

; Hack
;(fn [x] (str xx)) '(fn [x] (str x x))


; #127 # Love triangle #################################
;  Difficulty:	Hard
;  Topics:	search data-analysis
#_(fn [v]
  (let [b (reverse (take-while #(< % (apply max v)) (iterate #(* 2 %) 1)))
        m (map 
            #(reduce 
              (fn [s n]
                (if (< % (+ n (apply + (map * b s)))) (conj s 0) (conj s 1)))
              [] b)
            v)
        trp #(apply map vector %)
        res (remove nil?
              (for [m [m (trp m) (reverse m) (trp (reverse m))]      ; 4 rotations
                    [it-i it-n] [[+ inc] [dec #(+ % 2)]]   ; 2 ways to form a triangle
                    k (range (count m)) i (range (count (first m)))] ; start checking from every item
                      (loop [i i n 1 [r & m] (drop k m) sum 0]    ; start with 1 item
                        (if (= n (apply + (take n (drop i r))))   ; expand while n ones found
                            (recur (it-i i) (it-n n) m (+ n sum)) ; 
                            (if (< 1 sum) sum)))))]               ;
    (if (not (empty? res)) (apply max res))))
;[15 15 15 15 15]
;[1 3 7 15 31]


; #128 # Recognize playing cards ############################
;  Difficulty:	Easy
;  Topics:	strings game
;map (comp :rank
#_(fn [[s r]]
  {:suit ({\D :diamond \H :heart \C :club \S :space} s)
   :rank (- ({\T 10 \J 11 \Q 12 \K 13 \A 14} r (read-string (str r))) 2)})
;str) '[S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK SA]

; #130 # Tree reparenting ################################
;  Difficulty:	Hard
;  Topics:	tree
#_(fn [r t]
  ((fn f [[n & t] & anc]
    (remove nil?
      (if (= n r)
        (concat [n] t anc)
        (if (coll? t)
          (mapcat
            #(f (nth t %) (concat [n] (take % t) (drop (inc %) t) anc))
            (range (count t)))))))
  t))
;'n '(n)
;'a '(t (e) (a))
;'d '(a (b (c) (d) (e)) (f (g) (h)))
;'c '(a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))))

; #131 # Sum Some Set Subsets
;  Difficulty:	Medium
;  Topics:	math
#_(fn [& sets]
  (not (empty?
    (reduce 
      #(filter (set %1) %2)
      (map
        (fn [v]
          (reduce
            (fn [s i] (into s (cons i (map #(+ % i) s))))
            #{} v))
        sets)))))
;#{-1 1 99} #{-2 2 888} #{-3 3 7777}


; #132 # Insert between two items ################################
;  Difficulty:	Medium
;  Topics:	seqs core-functions
#_(fn [p pw v]
  (concat (take 1 v) 
    (mapcat 
      (fn [[a b]] (if (p a b) [pw b] [b])) 
      (partition 2 1 v))))
;< :less [1 6 7 4 3]
;> :more ()

; #137 # Digits and bases ########################################
;  Difficulty:	Medium
;  Topics:	math
#_(fn [v b]
  (loop [i v r []]
    (if (< i b)
      (cons i r)
      (recur (int (/ i b)) (cons (mod i b) r)))))
;1234501 10

; #138 # Squares Squared #####################################
;  Difficulty:	Hard
;  Topics:	data-analysis
#_(fn [a b]
  (let [; Create the data sequence v.
        v (apply str (map str (take-while #(<= % b) (iterate #(* % %) a))))
        ; Calculate the size of the result matrix.
        n (first (drop-while #(< (* % %) (count v)) (iterate inc 1)))
        ; Get diagonal units from a matrix.
        diag #(mapcat (fn [n] (take 1 (drop n (nth % n)))) (range (count %)))
        ; Rotate matrix 90 degs.
        rot90 #(reverse (apply map vector %))
        ; Create nxn spiral matrix from a sequence v.
        spir (fn [[i & is] v m]
                (if i
                  (recur is (drop i v) (rot90 (cons (take i v) m)))
                  m))
        ; Pretty output with 45 deg rotation.
        pretty (fn [m]
                (for [j (range (- 1 n) n)]
                  (apply str (concat
                    (repeat (max j (- j)) " ")
                    (rest (mapcat
                            #(str " " %)
                            (diag (map #(drop (- j) %) (drop j m)))))
                    (repeat (max j (- j)) " ")))))]
    (->>
      (spir (interleave (range n) (range 1 (inc n)))
            (concat v (repeat (* 2 n) \*)) [])
      (#(if (odd? n) (rot90 (rot90 %)) %))
      (pretty))))
;2 256


; #140 # Veitch, Please! #############################################
;  Difficulty:	Hard
;  Topics:	boolean-logic data-analysis
#_(fn [s]
  (let [neg '{a A A a b B B b c C C c d D D d}]
    (letfn [(butp [p r] (set (remove (set p) r)))
            (subset? [sub super] (every? #(contains? super %) sub))
            (covers? [r s] (empty? (remove (fn [k] (some #(subset? % k) r)) s)))
            (aggr-by [r p]
              (->>
                [(filter #(contains? % p) r) (filter #(contains? % (neg p)) r)]
                (map (fn [q] (map #(butp [p (neg p)] %) q)))
                (apply #(set (filter (set %1) %2)))
                (#(into % (remove (fn [k] (contains? % (butp [p (neg p)] k))) r)))))
            (branch [r p]
              (if (empty? p)
                r
                (set (mapcat #(branch (aggr-by r %) (butp [%] p)) p))))
            (cleanup [r k]
              (if (some #(subset? % k) r)
                r 
                (remove #(covers? (butp [%] (conj r k)) s) (conj r k))))]
    (set (reduce cleanup #{} (sort-by count(branch s (first s))))))))

; odOOboqggpodOObo q g g p o d O  O  b  o  q   g   g   p       o 

;#{#{'a 'B 'C 'd} #{'A 'b 'c 'd} #{'A 'b 'c 'D} #{'A 'b 'C 'd} #{'A 'b 'C 'D} #{'A 'B 'c 'd} #{'A 'B 'c 'D} #{'A 'B 'C 'd}}
;#{#{'a 'b 'c 'd} #{'a 'B 'c 'd} #{'A 'B 'c 'd} #{'a 'b 'c 'D} #{'a 'B 'c 'D} #{'A 'B 'c 'D}}
;#{#{'a 'b 'c 'd} #{'a 'B 'c 'd} #{'a 'b 'c 'D} #{'a 'B 'c 'D} #{'A 'B 'C 'd} #{'A 'B 'C 'D} #{'A 'b 'C 'd} #{'A 'b 'C 'D}}
;#{#{'a 'B 'c 'd} #{'A 'B 'c 'd} #{'a 'b 'c 'D} #{'a 'b 'C 'D} #{'A 'b 'c 'D} #{'A 'b 'C 'D} #{'a 'B 'C 'd} #{'A 'B 'C 'd}}



; #141 # Tricky card games ##################################
;  Difficulty:	Medium
;  Topics:	game cards
#_((fn [trump]
  (fn [cards]
    (let [order (zipmap [trump ((first cards) :suit)] (range))]
      (->>
        (sort-by #(order (% :suit) 2) cards)
        (partition-by #(order (% :suit) 2))
        (first)
        (sort-by #(% :rank))
        (last)))))
)


;nil) [{:suit :club :rank 4} {:suit :club :rank 9}]
;nil) [{:suit :spade :rank 2} {:suit :club :rank 10}]
;:club) [{:suit :spade :rank 2} {:suit :club :rank 10}]
;:heart) [{:suit :heart :rank 6} {:suit :heart :rank 8} {:suit :diamond :rank 10} {:suit :heart :rank 4}]



; #144 # Oscilrate ###############################################
;  Difficulty:	Medium
;  Topics:	sequences

;take 12 (
#_(fn [i & f]
  ((fn g [i n]
    (cons i (lazy-seq (g ((nth f (mod n (count f))) i) (inc n))))) i 0))
#_(fn [i & f] (reductions #(%2 %1) i (cycle f)))
;0 inc dec inc dec inc)


; #146 # Trees into tables ##########################################
;  Difficulty:	Easy
;  Topics:	seqs maps
#_(fn [m]
  (into {}
    (for [[ak av] m [bk bv] av] 
      [[ak bk] bv])))
;#(into {} (for [[k v] % [k2 v2] v] [[k k2] v2]))
;'{[1] {a b c d} [2] {q r s t u v w x}}

; #147 # Pascal's Trapezoid #########################################
;  Difficulty:	Easy
;  Topics:	seqs
;last (take 100 (

;iterate (fn [i] (vec (map (comp biginteger +) (cons 0 i) (conj i 0))))
#_(fn pascal [i]
  (cons (map biginteger i) 
        (lazy-seq 
          (pascal 
            (map 
              #(bigint (apply + %)) 
              (partition-all 2 1 (cons 0 i)))))))
;[2 4 2]))


; #148 # The Big Divide ##############################################
;  Difficulty:	Medium
;  Topics:	math
#_(fn [n a b]
  (let [av (take-while #(< % (* a b)) (iterate #(+ a %) a))
        bv (take-while #(< % (* a b)) (iterate #(+ b %) b))
        s (+ (apply + av) (apply + bv) (* a b))
        incr (* a b (+ 1 (count av) (count bv)))
        m (bigint (/ n (* a b)))]
    (+ (* (bigint (* (dec m) (/ m 2))) incr)
       (* m s)
       (apply + (take-while #(< % n) (iterate #(+ a %) (+ a (* m a b)))))
       (apply + (take-while #(< % n) (iterate #(+ b %) (+ b (* m a b))))))))

#_(fn [n a b]
  (letfn [(g [k] (/ (* k (bigint (inc k))) 2))
          (gsum [k] (* k (g (quot (dec n) k))))]
    (-
      (+ (gsum a) (gsum b))
      (gsum (* a b)))))
;1000 3 5
;3 17 11
;(* 10000 10000 1000) 1597 3571
;100000000 3 5


; #150 # Palindromic Numbers ##########################################
;  Difficulty:	Medium
;  Topics:	seqs math
;take 16 (
;first (
#_(fn f 
  ([n]
    (let [ls (apply str (take (quot (inc (count (str n))) 2) (str n)))
          u (apply * (cons 1 (repeat (count ls) 10)))]
      (drop-while #(< % n) (f (read-string ls) u (count (str n))))))
  ([l u c]  
    (if (odd? c)
      (lazy-cat 
          (map #(read-string (str % (apply str (rest (reverse (str %)))))) (range l u))
          (f (/ u 10) u (inc c)))
      (lazy-cat
          (map #(read-string (str % (apply str (reverse (str %))))) (range l u))
          (f u (* 10 u) (inc c))))))
;0)
;162)
;(* 111111111 111111111))



; #153 # Pairwise Disjoint Sets #########################################
;  Difficulty:	Easy
;  Topics:	set-theory
#_(apply distinct? (apply concat %))
#_(fn [s]
  (not-any?
    (fn [i]
      (some
        (fn [j] (some #(contains? j %) i))
        (remove #{i} s)))
    s))
;#{#{:a :b :c :d :e} #{:a :b :c :d} #{:a :b :c} #{:a :b} #{:a}}
;#{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}}



; #158 # Decurry #######################################################
;  Difficulty:	Medium
;  Topics:	partial-functions
#_(fn [f] (fn [& v] (reduce #(%1 %2) f v)))
#_((fn [f] 
  #(loop [f f [a & v] %&]
    (let [r (f a)]
      (if (fn? r) (recur r v) r))))
(fn [a] (fn [b] (fn [c] (fn [d] (+ a b c d))))))
; 1 2 3 4


#_(fn [s]
  (let [s (sort s)]
    (partition 2
      (concat
        (take 1 s)
        (apply concat (filter #(< (apply - %) -1) (partition 2 1 s)))
        (take-last 1 s)))))
;)
;[]
;[19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11]




; #177 # Balancing Brackets #####################################
;  Difficulty:	Medium
;  Topics:	parsing
#_(loop [[b & s] % [c & r :as a] []]
  (cond
    (nil? b) (empty? a)
    ((set "([{") b) (recur s (cons b a))
    (= c ((zipmap ")]}" "([{") b)) (recur s r)
    ((set ")]}") b) false
    :else (recur s a)))
;"([]([(()){()}(()(()))(([[0]]({}()))())]((((()()))))))"




; #152 # Latin Square Slicing ########################################
;  Difficulty:	Hard
;  Topics:	data-analysis math

#_(fn [v] 
  (let [maxn (apply max (map count v))
        pad (fn [n v] (take maxn (concat (repeat n nil) v (repeat nil))))
        latin? (fn [m]
                  (and (not-any? #(some nil? %) m)
                       (= (count m) (count (distinct (apply concat m))))
                       (every? #(apply distinct? %) (concat m (apply map vector m)))))
        aligms ((fn align [[i & w]]
                  (if (empty? w)
                    (map #(vector (pad % i)) (range (- (inc maxn) (count i))))
                    (mapcat
                      (fn [u] (map
                                #(cons (pad % i) u)
                                (range (- (inc maxn) (count i)))))
                      (align w)))) v)]
    (into {}
      (remove #(zero? (last %))
        (map
          (fn [z]
            [z (count (filter latin?
                  (reduce
                    #(into %1 (partition z 1 (apply map vector %2))) 
                    #{} (reduce #(into %1 (partition z 1 %2)) #{} aligms))))])
          (range 2 (inc (min (count v) maxn))))))))
;'[[A B C D] [A C D B] [B A D C] [D C A B]] ; {}
;'[[A B C D E F] [B C D E F A] [C D E F A B] [D E F A B C] [E F A B C D] [F A B C D E]] ; {6 1}
;'[[A B C D] [B A D C] [D C B A] [C D A B]] ; {4 1, 2 4})
;[[3 1 2] [1 2 3 1 3 4] [2 3 1 3]] ; {3 1, 2 2}
;[[8 6 7 3 2 5 1 4] [6 8 3 7] [7 3 8 6] [3 7 6 8 1 4 5 2] [1 8 5 2 4] [8 1 2 4 5]] ; {4 1, 3 1, 2 7}



; #168 # Infinite Matrix #########################################
;  Difficulty:	Medium
;  Topics:	seqs recursion math
;take 5 (map #(take 6 %) (
#_(letfn [(r [f m n] (lazy-seq (cons (f m n) (r f m (inc n)))))]
  (fn infm
    ([f] (infm f 0 0))
    ([f m n] (lazy-seq (cons (r f m n) (infm f (inc m) n))))
    ([f m n s t] (take s (map #(take t %) (infm f m n))))))

;str 3 5))
;(juxt bit-or bit-xor)
;(juxt quot mod) 13 21))



; #178 # Best Hand #################################################
;  Difficulty:	Hard
;  Topics:	strings game
#_(fn [hand]
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
;(= :high-card (__
;["HA" "D2" "H3" "C9" "DJ"]
;(= :pair (__ 
;["HA" "HQ" "SJ" "DA" "HT"]
;(= :two-pair (__
;["HA" "DA" "HQ" "SQ" "HT"]
;(= :three-of-a-kind (__ 
;["HA" "DA" "CA" "HJ" "HT"]
;(= :straight (__ 
;["HA" "DK" "HQ" "HJ" "HT"]
;(= :straight (__ 
;["HA" "H2" "S3" "D4" "C5"]
;(= :full-house (__ 
;["HA" "DA" "CA" "HJ" "DJ"]
;(= :four-of-a-kind (__ 
;["HA" "DA" "CA" "SA" "DJ"]
;(= :straight-flush (__ 
;["HA" "HK" "HQ" "HJ" "HT"]




; #164 # Language of a DFA #################################################
;  Difficulty:	Hard
;  Topics:	automata seqs
#_(fn [d]
  (letfn [(t [[k v]]
            (if-let [i ((d :transitions) k)]
              (map #(vector (second %) (str v (first %))) i)))
          (dfa [s]
            (if (first s)
              (lazy-cat
                (map second (filter #(contains? (d :accepts) (first %)) s))
                (dfa (mapcat t s)))))]
    (dfa [[(d :start) ""]])))

;'{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
;              :alphabet #{e h i l o y}
;              :start q0
;              :accepts #{q2 q4 q7}
;              :transitions {q0 {h q1}
;                            q1 {i q2, e q3}
;                            q3 {l q5, y q4}
;                            q5 {l q6}
;                            q6 {o q7}}}
