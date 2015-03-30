(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc b e]
          (if (zero? e)
            acc
            (recur (* acc b) b (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc b-seq]
                 (if (empty? b-seq)
                   acc
                 (recur (first b-seq) (rest b-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
           (empty? seq1) (empty? seq2)
           (empty? seq2) (empty? seq1)
           (not (== (first seq1) (first seq2))) false
           :else (recur (rest seq1) (rest seq2))
           ))

(defn find-first-index [pred a-seq]
  (loop [acc 0 h-pred pred h-seq a-seq]
    (cond
     (empty? h-seq) nil
     (pred (first h-seq)) acc
     :else (recur (inc acc) pred (rest h-seq))
     )))

(defn avg [a-seq]
  (loop [acc 0 cnt 0 h-seq a-seq]
    (if
     (empty? h-seq) (/ acc cnt)
     (recur (+ acc (first h-seq)) (inc cnt) (rest h-seq))
     )))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{} h-seq a-seq]
    (if
     (empty? h-seq) acc
     (recur (toggle acc (first h-seq)) (rest h-seq))
     )))

(defn fast-fibo [n]
  (loop [a 0 b 1 m n]
    (if
      (zero? m) a
      (recur b (+ a b) (dec m))
      )))

(defn cut-at-repetition [a-seq]
  (loop [seen #{} ret [] h-seq a-seq]
    (cond
     (empty? h-seq) ret
     (contains? seen (first h-seq)) ret
     :else (recur (conj seen (first h-seq)) (conj ret (first h-seq)) (rest h-seq))
     )))

