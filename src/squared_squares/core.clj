(ns squared-squares.core)

;; Create a function of two integer arguments: the start and end, respectively.
;; You must create a vector of strings which renders a 45° rotated square of integers
;; which are successive squares from the start point up to and including the end point.
;; If a number comprises multiple digits, wrap them around the shape individually.
;; If there are not enough digits to complete the shape, fill in the rest with asterisk
;; characters. The direction of the drawing should be clockwise, starting from the center of
;; the shape and working outwards, with the initial direction being down and to the right.


(defn odd-numbers []
  (drop 1 (filter odd? (range))))


;; 3 7 11 15 ... -> 0
;; 5 9 13 17 ... -> 1
                                        ;                                        ;
(defn fn-direction [n]
  (cond (= (rem n 4) 3) 0
        :else 1))
                                        ;
(defn direction []
  (lazy-seq (mapcat #(take % (repeat (fn-direction %))) (odd-numbers))))

(defn squares [n]
  (let [m (* n (bigint n))]
    (lazy-seq (concat (list n) (squares m)))))

(defn seq-squares [n m]
  (for [i (squares n) :while (<= i m)] i))

(defn int2l-r [n]
  (if (< n 10) (list n) (conj (int2l-r (quot n 10)) (rem n 10))))

(defn int2l [n]
  (reverse (int2l-r n)))

(defn seq-squares-* [n m]
  (let [sq (seq-squares n m)
        l (reduce + (map #(count (int2l %)) sq))]
    (take (next-square l) (map str (concat (mapcat int2l sq) (repeat \*))))))


(defn in? [coll x]
  (cond (nil? coll) false (= x (first coll)) true
        (< x (first coll)) false
        :else (in? (rest coll) x)))

(defn next-square [n]
  (let [sq (lazy-seq (map (fn [x] (* x x)) (range)))]
    (if (in? sq n) n (first (filter #(< n %) sq)))))

(defn do-draw [seq]
  seq)

(defn sqsq [n m]
  (cond (= n m) [(clojure.string/join (map str (int2l n)))]
        :else (do-draw (seq-squares-* n m))))


;; m = {:seq :line :dir :res}
;
(defn f [m]
  (cond (empty? (:res m)) (hash-map :seq (drop 1 (:seq m))
                                    :line (if (zero? (first (:dir m)))
                                            (inc (:line m))
                                            (dec (:line m)))
                                    :dir (drop 1 (:dir m))
                                    :res [(str (first (:seq m)))])
        :else :x))
