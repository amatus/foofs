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

(ns foofs.filesystem
  (:use foofs.util))

;; These are the operations that can be preformed on the filesystem "database".
(defprotocol Filesystem
  "A Foofs filesystem."
  (lookup [this inode child continuation!]
          "Lookup the inode of a directory entry.")
  (getattr [this inode continuation!]
           "Get the attributes of an inode.")
  (reference [this inode continuation!]
             "Increment link count of an inode.")
  (dereference [this inode continuation!]
               "Decrement link count of an inode.")
  (clonedir [this inode continuation!]
            "Return a lazy sequence of directory entries.")
  ;; and so on
  )

(defn link-modifier!
  [state-agent f inode continuation! state]
  (let [attr-table (:attr-table state)
        attr (get attr-table inode)]
    (if (nil? attr)
      (do
        (continuation!)
        state)
      (do
        (send state-agent
              (fn [state]
                (continuation!)
                state))
        (assoc-deep state (f (:link attr)) :attr-table inode :link)))))

(defrecord FooFilesystem
  [^clojure.lang.Agent state-agent]
  Filesystem
  (lookup [this inode child continuation!]
    (let [state (deref state-agent)
          lookup-table (:lookup-table state)
          children (get lookup-table inode)
          child (if (= "" child)
                  inode
                  (get children child))
          attr-table (:attr-table state)
          attr (get attr-table child)]
      (if (nil? attr)
        (continuation! nil)
        (continuation! (assoc attr :inode child)))))
  (getattr [this inode continuation!]
    (let [attr-table (:attr-table (deref state-agent))
          attr (get attr-table inode)]
      (continuation! attr)))
  (reference [this inode continuation!]
    (send state-agent
          (partial link-modifier! state-agent inc inode continuation!)))
  (dereference [this inode continuation!]
    (send state-agent
          (partial link-modifier! state-agent dec inode continuation!)))
  (clonedir [this inode continuation!]
    (let [state (deref state-agent)
          lookup-table (:lookup-table state)
          children (get lookup-table inode)
          attr-table (:attr-table state)]
      (continuation!
        (map
          (fn [kv]
            (let [[name inode] kv]
              (conj {:name name
                     :inode inode}
                    (get attr-table inode))))
          children)))))

