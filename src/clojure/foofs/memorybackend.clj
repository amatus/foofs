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

(ns foofs.memorybackend
  (:use [foofs.filesystembackend :only [FilesystemBackend]]
        foofs.util))

(defn link-modifier!
  [state-agent f inode continuation! state]
  (let [attr-table (:attr-table state)
        attr (get attr-table inode)]
    (if (nil? attr)
      (do
        (continuation! nil)
        state)
      (let [link (f (:nlink attr))]
        (send state-agent
              (fn [state]
                (continuation! link)
                state))
        (assoc-deep state link :attr-table inode :nlink)))))

(defrecord MemoryBackend
  [^clojure.lang.Agent state-agent]
  FilesystemBackend
  (lookup [this inode child continuation!]
    (.println *err* (str "lookup " inode " " child))
    (let [state (deref state-agent)
          lookup-table (:lookup-table state)
          children (get lookup-table inode)
          child (if (= "" child)
                  inode
                  (get children child))]
      (.println *err* (str "child " child))
      (continuation! child)))
  (getattr [this inode continuation!]
    (.println *err* (str "getattr " inode))
    (let [attr-table (:attr-table (deref state-agent))
          attr (get attr-table inode)]
      (.println *err* (str "attr " attr))
      (continuation! attr)))
  (reference [this inode continuation!]
    (.println *err* (str "reference " inode))
    (send state-agent
          (partial link-modifier! state-agent inc inode continuation!)))
  (dereference [this inode continuation!]
    (.println *err* (str "dereference " inode))
    (send state-agent
          (partial link-modifier! state-agent dec inode continuation!)))
  (clonedir [this inode continuation!]
    (.println *err* (str "clonedir " inode))
    (let [state (deref state-agent)
          lookup-table (:lookup-table state)
          children (get lookup-table inode)
          attr-table (:attr-table state)]
      (continuation!
        (map
          (fn [kv]
            (let [[name inode] kv
                  attr (get attr-table inode)]
              {:name name
               :nodeid inode
               :type (:mode attr)}))
          children))))
  (readfile [this inode offset size continuation!]
    (let [state (deref state-agent)
          file (get (:file-table state) inode)]
      (if (nil? file)
        (continuation! nil)
        (continuation! (take size (drop offset file)))))))
