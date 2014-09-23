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

(deftest map-pattern
  (is (= (match {:foo 4 :bar 7}
           (u/map :foo x :bar y) [x y])
         [4 7])))

(deftest guard
  (is (= (match 1
           (u/when x string?) :foo
           (u/when x number? pos?) :bar)
         :bar)))
