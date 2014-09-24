(ns unapply.test
  (:require [clojure.test :refer :all]
            [unapply.core :refer [match] :as u]))

(with-test
  (defn sum [xs]
    (match xs
      (u/seq) 0
      (u/seq x xs') (+ x (sum xs'))))
  (is (= (sum [1 2 3]) 6))
  (is (= (sum [2 4 8]) 14)))

(with-test
  (defn numbers? [xs]
    (match xs
      (u/seq) true
      (u/seq (u/when _ number?) xs') (recur xs')
      _ false))
  (is (numbers? [1 2 3]))
  (is (not (numbers? [1 "2" 3]))))

(deftest seq-pattern
  (is (= (match [1 2 3]
           (u/seq) nil
           (u/seq h t) [h t])
         [1 [2 3]])))

(deftest map-pattern
  (is (= (match {:foo 4 :bar 7}
           (u/map :foo x :bar y) [x y])
         [4 7])))

(deftest guard
  (is (= (match :foo
           (u/when _ string?) :bar
           (u/when _ keyword?) :baz)
         :baz)))
