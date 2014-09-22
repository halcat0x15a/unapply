(ns unapply.core
  (:refer-clojure :exclude [seq])
  (:require [clojure.core :as core]))

(declare match)

(defmacro unapply [e f tests result clauses]
  (let [var (gensym)]
    `(let [~var (~f ~e)]
       (if (= ~(count tests) (count ~var))
         ~(reduce (fn [a [e test]]
                    `(match (nth ~var ~e) ~test ~a ~@clauses))
                  result
                  (reverse (map-indexed vector tests)))
         (match ~e ~@clauses)))))

(defmacro match [e & clauses]
  (if clauses
    (if (next clauses)
      (let [test (first clauses)
            result (second clauses)
            clauses (nnext clauses)]
        (cond (seq? test) `(unapply ~e ~(first test) ~(next test) ~result ~clauses)
              (symbol? test) `(let [~test ~e] ~result)
              :else `(if (= ~e ~test) ~result)))
      `(throw (IllegalArgumentException. "match requires an even number of clauses")))))

(defn seq [x]
  (if (sequential? x)(vec x)))

(defn number [x]
  (if (number? x) [x]))
