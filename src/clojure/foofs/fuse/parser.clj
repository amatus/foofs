(ns foofs.fuse.parser
  (:use clojure.contrib.monads))

(def parser-m (state-t maybe-m))

(defn parser-m-until
  "An optimized implementation of m-until for the parser monad that
   replaces recursion by a loop."
  [p f x]
  (letfn [(until [p f x s]
            (if (p x)
              [x s]
              (when-let [xs ((f x) s)]
                (recur p f (first xs) (second xs)))))]
    (fn [s] (until p f x s))))

(defn parse-int32
  [buffer]
  (when (<= 4 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(.getInt buffer2) buffer2])))

(defn parse-uint32
  [buffer]
  (when (<= 4 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(bit-and 0xffffffff (.getInt buffer2)) buffer2])))

(defn parse-int64
  [buffer]
  (when (<= 8 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(.getLong buffer2) buffer2])))

(def parse-opaque64 parse-int64)

(defn parse-uint64
  [buffer]
  (when (<= 8 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(.and (biginteger 0xffffffffffffffff) (biginteger (.getLong buffer2)))
       buffer2])))

(with-monad parser-m

  (defn optional
    "Makes a parser optional."
    [parser]
    (m-plus parser (m-result nil)))

  (defn none-or-more
    "Makes a parser repeat none or more times."
    [parser]
    (fn [s]
      (let [xs ((parser-m-until
                      first
                      #(fn [s]
                         (if-let [x (parser s)]
                           [[false (conj (second %) (first x))] (second x)]
                           [[true (second %)] s]))
                      [false []]) s)]
        [(second (first xs)) (second xs)])))

  (defn one-or-more
    "Makes a parser repeat one or more times."
    [parser]
    (domonad
      [x parser
       xs (none-or-more parser)]
      (cons x xs)))

  (defn n-times
    "Makes a parser repeat exactly n times."
    [parser n]
    (fn [s]
      (when-let [xs ((parser-m-until
                      #(<= n (first %))
                      #(fn [s]
                         (when-let [xs (parser s)]
                           [[(inc (first %)) (conj (second %) (first xs))]
                            (second xs)]))
                      [0 []]) s)]
        [(second (first xs)) (second xs)])))
)
