(ns squared-squares.core-test
  (:use clojure.test
        squared-squares.core))

(deftest squared-squares-test
  (testing "squared-squares-test failed."

    (is (= (sqsq 2 2) ["2"]))

    (is (= (sqsq 2 4)
           [" 2 "
            "* 4"
            " * "]))

    (is (= (sqsq 3 81)
           [" 3 "
            "1 9"
            " 8 "]))

    (is (= (sqsq 4 20)
           [" 4 "
            "* 1"
            " 6 "]))

    (is (= (sqsq 2 256)
           ["  6  "
            " 5 * "
            "2 2 *"
            " 6 4 "
            "  1  "]))

    (is (= (sqsq 10 10000)
           ["   0   "
            "  1 0  "
            " 0 1 0 "
            "* 0 0 0"
            " * 1 * "
            "  * *  "
            "   *   "]))))
