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
      (u/cons x xs') (+ x (sum xs'))))
  (is (= (sum [1 2 3]) 6))
  (is (= (sum [2 4 8]) 14)))

(with-test
  (defn zip [xs ys]
    (match [xs ys]
      [() ()] ()
      [(u/cons x xs') (u/cons y ys')] (cons [x y] (zip xs' ys'))))
  (is (= (zip [:a :b :c] [1 2 3])
         [[:a 1] [:b 2] [:c 3]]))
  (is (= (zip [] []) ())))

(with-test
  (defn succ [x]
    (if (and (integer? x) (pos? x))
      [(dec x)]))
  (letfn [(factrial [n]
            (match n
              0 1
              (succ m) (* n (factrial m))))]
    (is (= (factrial 5) 120))))
