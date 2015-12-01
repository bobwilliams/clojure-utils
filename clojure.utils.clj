; math related functions

(defn exp [x n]
  (reduce *' (repeat n x)))

(defn factorial [n] (reduce *' (range 1 (inc n))))

(def fibo (map first (iterate (fn [[a b]] [b (+' a b)]) [0 1])))

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

(defn lazy-primes []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve n]            
            (if-let [step (sieve n)]
              (-> sieve
                  (dissoc n)    
                  (enqueue n step))
              (enqueue sieve n (+ n n))))
          (next-primes [sieve n]
            (if (sieve n) 
              (recur (next-sieve sieve n) (+ n 2))
              (cons n (lazy-seq (next-primes (next-sieve sieve n) (+ n 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(defn abs-val [n]
  (if (> 0 n) (* -1 n) n))

(defn collatz [n]
  (cons n
        (lazy-seq
         (if (> n 1)
           (if (even? n)
                (collatz (/ n 2))
                (collatz (+ (* 3 n) 1)))))))

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

(defn apply-fn-to-digits [x n]
  (apply x (map #(- (int %) 48) (seq (str n)))))

(defn powers-of-n [n]
  (iterate (partial *' n) 1))

(defn proper-divisors [n]
  (filter #(zero? (mod n %)) (range 1 (+ 1 (/ n 2)))))

(defn perfect? [n]
  (= n (reduce + (proper-divisors n))))

(defn abundant? [n]
  (let [sum (reduce + (proper-divisors n))]
    (< n sum)))

(defn to-digits [i]
  (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9} (str i))) 

; string related functions

(use '[clojure.string :only (join split capitalize lower-case)])

(defn title-case [s]
  "Given a string, returns a string with each word (separated by a space) captialized and all other letters lower-case."
  (join " " (map #(capitalize  %) (split (lower-case s) #"\s"))))

(defn in? 
  [seq elm]  
  (some #(= elm %) seq))
