; Copyright (C) David Barksdale 2012 <amatus.amongus@gmail.com>
;
; foofs is free software: you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by the
; Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; foofs is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; See the GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License along
; with this program.  If not, see <http://www.gnu.org/licenses/>.

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
