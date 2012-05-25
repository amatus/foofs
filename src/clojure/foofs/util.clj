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
