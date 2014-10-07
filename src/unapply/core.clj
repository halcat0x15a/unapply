(ns unapply.core
  (:refer-clojure :exclude [cons])
  (:require [clojure.core :as core]))

(declare match)

(defmacro unapply [e extractor patterns result clauses]
  (let [vals (gensym)]
    `(let [~vals (~extractor ~e)]
       (if (identical? (count ~vals) ~(count patterns))
         ~(reduce (fn [result [n pattern]]
                    `(match (nth ~vals ~n) ~pattern ~result _# (match ~e ~@clauses)))
                  result
                  (reverse (map-indexed vector patterns)))
         (match ~e ~@clauses)))))

(defmacro match [e & clauses]
  (if clauses
    (if (next clauses)
      (let [pattern (first clauses)
            result (second clauses)
            clauses (nnext clauses)]
        (cond (and (seq? pattern) (first pattern)) `(unapply ~e ~(first pattern) ~(next pattern) ~result ~clauses)
              (vector? pattern) `(unapply ~e identity ~pattern ~result ~clauses)
              (symbol? pattern) `(let [~pattern ~e] ~result)
              :else `(if (= ~e ~pattern) ~result (match ~e ~@clauses))))
      (throw (IllegalArgumentException. "match requires an even number of clauses")))))

(defn cons [x]
  (if (and (sequential? x) (first x))
    [(first x) (rest x)]))
