(ns squares-squared.core
  (:gen-class))

(defn squares-squared [a b]
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

(defn -main
  "Printing out squares squared."
  [& args]
  (->>
    (map read-string args)
    (apply squares-squared)
    (map #(str % "\n"))
    (println)))
