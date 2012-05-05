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

(defn assoc-deep
  "Associates val with the 'path' of keys in a nested map."
  [_map _val _key & _keys]
  (if (nil? _keys)
    (assoc _map _key _val)
    (assoc _map _key (apply assoc-deep (get _map _key) _val _keys))))

(defn next-key
  [_map _key _min _max]
  (first (remove (partial contains? _map) (concat (range _key _max)
                                                  (range _min _key)))))

(defn skip
  [& _])

(defn chain
  ([] nil)
  ([fn] (fn))
  ([fn & fns] (fn (partial chain fns))))
