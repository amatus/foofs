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

(def new-attr
  {:size 0
   :blocks 0
   :atime 0
   :mtime 0
   :ctime 0
   :atimensec 0
   :mtimensec 0
   :ctimensec 0
   :mode 0
   :nlink 0
   :uid 0
   :gid 0
   :rdev 0})

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
      (if (nil? attr)
        (continuation! nil)
        (continuation! (assoc attr :inode inode)))))
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
        (continuation! (take size (drop offset file))))))
  (mknod [this inode filename mode flags continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              children (get lookup-table inode)
              attr-table (:attr-table state)]
          (if (contains? children filename)
            (do
              (continuation! nil) ;; EEXIST?
              state)
            (let [child-inode (next-key attr-table (:next-inode state)
                                        Long/MIN_VALUE Long/MAX_VALUE)
                  attr (conj new-attr
                             {:mode mode
                              :nlink 1})]
              (send
                state-agent
                (fn [state]
                  (continuation! (assoc attr :inode child-inode))
                  state))
              (assoc state
                     :attr-table (assoc attr-table child-inode attr)
                     :lookup-table (assoc lookup-table inode
                                          (assoc children filename child-inode))
                     :next-inode (inc child-inode)))))))))
