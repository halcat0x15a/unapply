(ns unapply.core
  (:refer-clojure :exclude [vec seq map when])
  (:require [clojure.core :as core])
  (:import [java.util.regex Pattern]))

(declare match)

(defprotocol Eq
  (eq [x y]))

(extend-protocol Eq
  Pattern
  (eq [x y]
    (and (instance? Pattern y)
         (= (.pattern x)
            (.pattern ^Pattern y))))
  Object
  (eq [x y]
    (= x y))
  nil
  (eq [x y]
    (nil? y)))

(defmacro unapply [e f patterns result clauses]
  (let [vals (gensym)]
    `(let [~vals ((~f ~patterns) ~e)]
       (if (identical? ~(count patterns) (count ~vals))
         ~(reduce (fn [a [i pattern]]
                    `(match (nth ~vals ~i) ~pattern ~a _# (match ~e ~@clauses)))
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
              :else `(if (eq ~e ~pattern) ~result (match ~e ~@clauses))))
      `(throw (IllegalArgumentException. "match requires an even number of clauses")))))

(defmacro vec [patterns]
  '(fn [e]
     (if (sequential? e)
       e)))

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
         (if (and (map? ~e) ~@(core/map (fn [k] `(contains? ~e ~k)) keys))
           ~(reduce (fn [v k] (conj v k `(get ~e ~k))) [] keys))))
    (throw (RuntimeException. "map must contain an even number of forms"))))

(defmacro when [patterns]
  (let [e (gensym)
        tests (next patterns)]
    `(fn [~e]
       (if (and ~@(core/map (fn [p] `(~p ~e)) tests))
         [~e ~@tests]))))

(defmacro regex [patterns]
  `(fn [e#]
     (let [re# ~(first patterns)]
       (if-let [result# (re-matches re# e#)]
         (cons re# (next result#))))))
