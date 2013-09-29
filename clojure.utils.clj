; math related functions

(def fibo (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn factor? [n d] 
	(if (zero? d)
		false
		(zero? (mod n d))))

(defn gcd [a b] 
  (if (zero? b) 
    a 
    (recur b (mod a b))))

(defn lcm [a b] 
  (/ (* a b) (gcd a b)))

(defn square [n]
  (* n n))

(defn sum-of-squares [n]
  (reduce + (map #(square %) (range 1 (+ n 1)))))

(defn square-of-sums [n]
  (square (reduce + (range 1 (+ n 1)))))

(defn prime? [n]
  (cond
    (< n 2) false
    (< n 4) true
    :else (not= 0 (reduce *' (map #(mod n %) (range 2 (+ 1 (Math/sqrt n))))))))



; string related functions

(use '[clojure.string :only (join split capitalize lower-case)])

(defn title-case [s]
  "Given a sentence, returns a string with each word captialized and all other letters lower-case."
  (join " " (map #(capitalize  %) (split (lower-case s) #"\s"))))