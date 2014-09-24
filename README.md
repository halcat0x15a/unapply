# Unapply

Scala-like pattern matching library for Clojure.

## Usage

Examples:

```clojure
(require '[unapply.core :refer [match] :as u])

(defn sum [xs]
  (match xs
    (u/seq) 0
    (u/seq x xs') (+ x (sum xs'))))

(defn zip [xs ys]
  (match [xs ys]
    (u/vec (u/seq) (u/seq)) '()
    (u/vec (u/seq x xs') (u/seq y ys')) (cons [x y] (zip xs' ys'))))
```

You can make extractors:

```clojure
(defmacro succ [patterns]
  '(fn [e]
     (if (and (integer? e) (pos? e))
       [(dec e)])))

(defn factrial [n]
  (match n
    0 1
    (succ m) (* n (factrial m))))
```

## License

Copyright © 2014 Sanshiro Yoshida.

Distributed under the Eclipse Public License.
