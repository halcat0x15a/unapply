(ns unapply.test
  (:require [clojure.test :refer :all]
            [unapply.core :refer [match] :as u]))

(with-test
  (defn fib [n]
    (match n
      0 0
      1 1
      _ (+ (fib (- n 1)) (fib (- n 2)))))
  (is (= (fib 5) 5))
  (is (= (fib 7) 13)))

(with-test
  (defn sum [xs]
    (match xs
      () 0
      (u/seq* x xs') (+ x (sum xs'))))
  (is (= (sum [1 2 3]) 6))
  (is (= (sum [2 4 8]) 14)))

(with-test
  (defn numbers? [xs]
    (match xs
      () true
      (u/seq* (u/when _ number?) xs') (recur xs')
      _ false))
  (is (numbers? [1 2 3]))
  (is (not (numbers? [1 \2 3]))))

(with-test
  (defn zip [xs ys]
    (match [xs ys]
      (u/seq () ()) ()
      (u/seq (u/seq* x xs') (u/seq* y ys')) (cons [x y] (zip xs' ys'))))
  (is (= (zip [:a :b :c] [1 2 3])
         [[:a 1] [:b 2] [:c 3]]))
  (is (= (zip [] []) ())))

(deftest map-pattern
  (is (= (match {:foo 4 :bar 7}
           (u/map :baz a) a
           (u/map :foo x :bar y) [x y])
         [4 7])))

(deftest regex-pattern
  (is (= (match "012-3456"
           (u/regex #"(\d{3})-(\d{4})" x y) [x y])
         ["012" "3456"])))

(defmacro succ [patterns]
  `(fn [e#]
     (if (and (integer? e#) (pos? e#))
       [(dec e#)])))

(with-test
  (defn factrial [n]
    (match n
      0 1
      (succ m) (* n (factrial m))))
  (is (= (factrial 5) 120)))
