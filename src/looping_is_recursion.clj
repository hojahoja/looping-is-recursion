(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [fst lst]
                 (if (empty? lst)
                   fst
                   (recur (first lst) (rest lst))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
     true
   (or (empty? seq1) (empty? seq2))
     false
   (= (first seq1) (first seq2))
     (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         pr pred
         as a-seq]
    (cond
     (empty? as) nil
     (pr (first as)) i
     :else (recur (inc i) pr (rest as)))))

(defn avg [a-seq]
  (loop [size 0
         sum 0
         as a-seq]
    (if (empty? as)
      (/ sum size)
      (recur (inc size) (+ sum (first as)) (rest as)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [newset #{}
           as a-seq]
      (if (empty? as)
        newset
        (recur (toggle newset (first as)) (rest as))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         c n]
    (cond
     (= c 0)
      a
     (> 2 c)
     b
     :else (recur b (+ a b) (dec c)))))

(defn cut-at-repetition [a-seq]
  (loop [as a-seq gathering []]
    (cond
     (empty? as) a-seq
     (contains? (set gathering) (first as)) gathering
     :else (recur (rest as) (conj gathering (first as))))))

