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

(ns foofs.util)

(defn next-key
  [_map _key _min _max]
  (first (remove (partial contains? _map) (concat (range _key _max)
                                                  (range _min _key)))))

(defn skip
  [& _])

(defn chain
  ([] nil)
  ([f] (f))
  ([f & fs] (f (partial apply chain fs))))

(defmacro agent-do
  [_agent f]
  `(send ~_agent #(do ~f %)))

(def base32-alphabet [\A \B \C \D \E \F \G \H \I
                      \J \K \L \M \N \O \P \Q \R
                      \S \T \U \V \W \X \Y \Z \2
                      \3 \4 \5 \6 \7])

(defn to-32
  [x]
  (nth base32-alphabet (bit-and 0x1f x)))
 
(defn base32-encode
  [byte-seq]
  (mapcat (fn
            [[a b c d e]]
            [(to-32 (bit-shift-right a 3))
             (to-32 (bit-or (bit-shift-left a 2) (bit-shift-right b 6)))
             (to-32 (bit-shift-right b 1))
             (to-32 (bit-or (bit-shift-left b 4) (bit-shift-right c 4)))
             (to-32 (bit-or (bit-shift-left c 1) (bit-shift-right d 7)))
             (to-32 (bit-shift-right d 2))
             (to-32 (bit-or (bit-shift-left d 3) (bit-shift-left e 5)))
             (to-32 e)])
          (partition 5 5 (repeat (byte 0)) byte-seq)))
