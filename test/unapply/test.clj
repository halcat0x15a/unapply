(ns unapply.test
  (:require [clojure.test :refer :all]
            [unapply.core :refer [match] :as u]))

(deftest patterns
  (testing "primitive pattern"
    (is (= (match "foo"
             "bar" :bar
             "foo" :foo)
           :foo)))
  (testing "tree pattern"
    (is (= (match [1 2 3]
             (u/seq x y z) y)
           2)))
  (testing "nested pattern"
    (is (= (match [1 [2 3]]
             (u/seq x (u/seq y z)) z)
           3))))
