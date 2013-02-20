;(ns squared-squares.core)

;; Create a function of two integer arguments: the start and end, respectively.
;; You must create a vector of strings which renders a 45° rotated square of integers
;; which are successive squares from the start point up to and including the end point.
;; If a number comprises multiple digits, wrap them around the shape individually.
;; If there are not enough digits to complete the shape, fill in the rest with asterisk
;; characters. The direction of the drawing should be clockwise, starting from the center of
;; the shape and working outwards, with the initial direction being down and to the right.


(defn odd-numbers []
  (lazy-seq (drop 1 (filter odd? (range)))))

;; 3 7 11 15 ... -> 0
;; 5 9 13 17 ... -> 1
;;
(defn fn-direction [n]
  (cond (= (rem n 4) 3) 0
        :else 1))
;;
(defn direction []
  (lazy-seq (mapcat #(take % (repeat (fn-direction %))) (take 20 (odd-numbers)))))

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


;; res = [[1][2 4][1 6 *][* *][*]]
;;
(defn do-res [m]
  (let [item (first (:seq m))
        curr-line (first (:line m))
        curr-dir (first (:dir m))
        curr-res (:res m)]
    (cond (empty? curr-res) [[item]]
          (< curr-line 0) (vec (cons [item] curr-res))
          (< curr-line (count curr-res))
          (if (= curr-dir 0)
            (assoc-in curr-res [curr-line]
                      (conj (curr-res curr-line) item))
            (assoc-in curr-res [curr-line]
                      (vec (concat [item] (curr-res curr-line)))))
          :else (conj curr-res [item]))))

(defn do-draw [m]
  (println m)
  (let [new-seq (drop 1 (:seq m))
        new-line (drop 1 (:line m))
        new-dir (drop 1 (:dir m))]
    (cond (empty? (:seq m)) (:res m)
          (empty? (:res m)) (do-draw (hash-map :seq new-seq
                                               :line new-line
                                               :dir new-dir
                                               :res [[(str (first (:seq m)))]]))
          :else (do-draw (hash-map :seq new-seq
                                   :line new-line
                                   :dir new-dir
                                   :res (do-res m))))))

(defn sqsq [n m]
  (let [line '(0 1 2 1 0 -1 -1 1 2 3 4 5 6 5 4 3)
        seq (seq-squares-* n m)
        length (count seq)
        dir (take length (direction))]
    (cond (= n m) [(clojure.string/join (map str (int2l n)))]
          :else (do-draw {:res nil
                          :dir dir
                          :line line
                          :seq seq}))))
