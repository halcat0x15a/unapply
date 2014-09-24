(ns unapply.core
  (:refer-clojure :exclude [seq map when])
  (:require [clojure.core :as core]))

(declare match)

(defmacro unapply [e f patterns result clauses]
  (let [vals (gensym)]
    `(let [~vals ((~f ~patterns) ~e)]
       (if (= ~(count patterns) (count ~vals))
         ~(reduce (fn [a [i pattern]]
                    `(match (nth ~vals ~i) ~pattern ~a ~@clauses))
                  result
                  (reverse (map-indexed vector patterns)))
         (match ~e ~@clauses)))))

(defmacro match [e & clauses]
  (if clauses
    (if (next clauses)
      (let [pattern (first clauses)
            result (second clauses)
            clauses (nnext clauses)]
        (cond (seq? pattern) `(unapply ~e ~(first pattern) ~(next pattern) ~result ~clauses)
              (symbol? pattern) `(let [~pattern ~e] ~result)
              :else `(if (= ~e ~pattern) ~result (match ~e ~@clauses))))
      `(throw (IllegalArgumentException. "match requires an even number of clauses")))))

(defmacro seq [patterns]
  (let [n (dec (count patterns))]
    `(fn [e#]
       (if (and (sequential? e#) (pos? (count e#)))
         (concat (take ~n e#) [(nthnext e# ~n)])))))

(defmacro map [patterns]
  (if (even? (count patterns))
    (let [e (gensym)
          keys (core/map first (partition 2 patterns))]
      `(fn [~e]
         (if (map? ~e)
           ~(reduce (fn [v k] (conj v k `(get ~e ~k))) [] keys))))
    (throw (RuntimeException. "map must contain an even number of forms"))))

(defmacro when [patterns]
  (let [e (gensym)
        tests (next patterns)]
    `(fn [~e]
       (if (and ~@(core/map (fn [p] `(~p ~e)) tests))
         [~e ~@tests]))))
