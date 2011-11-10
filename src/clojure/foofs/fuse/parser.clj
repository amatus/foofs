(ns foofs.fuse.parser
  (:use clojure.contrib.monads))

(defmonad buffer-parser-m
  "Monad for parsing java.nio.ByteBuffer and the like. It is a parsers
  responsability to make a duplicate of the buffer before mutating it."
  [m-result (fn m-result-buffer-parser [v]
              (fn [buffer] [v (.duplicate buffer)]))
   m-bind   (fn m-bind-buffer-parser [parser f]
              (fn [buffer]
                (let [result (parser buffer)]
                  (if (nil? result)
                    nil
                    (let [[v buffer2] result]
                      ((f v) buffer2))))))
   m-zero   (fn m-zero-buffer-parser [buffer]
              nil)
   m-plus   (fn m-plus-buffer-parser [& parsers]
              (fn [buffer]
                (first
                  (drop-while nil? (map #(% buffer) parsers)))))])

(defn buffer-parser-m-until
  "An optimized implementation of m-until for the buffer-parser monad that
   replaces recursion by a loop."
  [p f x]
  (letfn [(until [p f x buffer]
            (if (p x)
              [x buffer]
              (when-let [xs ((f x) buffer)]
                (recur p f (first xs) (second xs)))))]
    (fn [buffer] (until p f x buffer))))

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

(with-monad buffer-parser-m

  (defn optional
    "Makes a parser optional."
    [parser]
    (m-plus parser (m-result nil)))

  (defn none-or-more
    "Makes a parser repeat none or more times."
    [parser]
    (fn [buffer]
      (let [xs ((buffer-parser-m-until
                      first
                      #(fn [buffer]
                         (if-let [x (parser buffer)]
                           [[false (conj (second %) (first x))] (second x)]
                           [[true (second %)] buffer]))
                      [false []]) buffer)]
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
    (fn [buffer]
      (when-let [xs ((buffer-parser-m-until
                      #(<= n (first %))
                      #(fn [buffer]
                         (when-let [xs (parser buffer)]
                           [[(inc (first %)) (conj (second %) (first xs))]
                            (second xs)]))
                      [0 []]) buffer)]
        [(second (first xs)) (second xs)])))
)
