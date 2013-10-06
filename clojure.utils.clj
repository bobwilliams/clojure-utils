; math related functions

(def fibo (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

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

(defn primes [end]
  "Uses a sieve to generate primes; not the set is not sorted"
  (let [sieve (transient (set (cons 2 (range 3 end 2))))]
    (loop [s sieve
           f 3]
      (cond (> (square f) end) (persistent! s)
            :else (recur (reduce disj! s (range (square f) end f)) (inc f))))))

(defn triangle [end]
  (reduce + (take end (iterate inc 1))))

(defn factors [num]
  (filter #(factor? num %) (range 1 (+ 1 num))))

(defn factor? [n d] 
  (if (zero? d)
    false
    (zero? (mod n d))))

(defn prime? [n]
  (cond
    (< n 2) false
    (< n 4) true
    :else (not= 0 (reduce *' (map #(mod n %) (range 2 (+ 1 (Math/sqrt n))))))))

(defn pythagorean-triplet? [a b c]
  (true? (and (= (< a b) (< b c))
     (= (+ (square a) (square b)) (square c)))))

; string related functions

(use '[clojure.string :only (join split capitalize lower-case)])

(defn title-case [s]
  "Given a string, returns a string with each word (separated by a space) captialized and all other letters lower-case."
  (join " " (map #(capitalize  %) (split (lower-case s) #"\s"))))