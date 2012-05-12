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
        foofs.fuse.jna
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

(defn attr-modifier!
  [state-agent inode f continuation!]
  (send
    state-agent
    (fn [state]
      (let [attr-table (:attr-table state)
            attr (get attr-table inode)]
        (if (nil? attr)
          (do (continuation! errno-noent) state)
          (let [new-attr (f attr)]
            (agent-do state-agent (continuation! new-attr))
            (assoc-deep state new-attr :attr-table inode)))))))

;; TODO rename s/attr/attrs/ almost everywhere
(defn attr-attribute-modifier!
  [state-agent inode f attribute continuation!]
  (attr-modifier! state-agent inode
                  (fn [attr]
                    (assoc attr attribute (f (get attr attribute))))
                  continuation!))

(defrecord MemoryBackend
  [^clojure.lang.Agent state-agent]
  FilesystemBackend
  (lookup [this inode child continuation!]
    (let [state (deref state-agent)
          lookup-table (:lookup-table state)
          children (get lookup-table inode)
          child (if (= "" child)
                  inode
                  (get children child))]
      (continuation! child)))
  (getattr [this inode continuation!]
    (let [attr-table (:attr-table (deref state-agent))
          attr (get attr-table inode)]
      (if (nil? attr)
        (continuation! nil)
        (continuation! (assoc attr :inode inode)))))
  (reference [this inode continuation!]
    (attr-attribute-modifier! state-agent inode inc :nlink continuation!))
  (dereference [this inode continuation!]
    (attr-attribute-modifier! state-agent inode dec :nlink continuation!))
  (clonedir [this inode continuation!]
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
  (mknod [this inode filename mode continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              children (get lookup-table inode)
              attr-table (:attr-table state)]
          (if (contains? children filename)
            (do
              (continuation! errno-exist)
              state)
            (let [child-inode (next-key attr-table (:next-inode state)
                                        Long/MIN_VALUE Long/MAX_VALUE)
                  attr (conj new-attr
                             {:mode mode
                              :nlink 1})]
              (agent-do state-agent
                        (continuation! (assoc attr :inode child-inode)))
              (assoc state
                     :attr-table (assoc attr-table child-inode attr)
                     :lookup-table (assoc lookup-table inode
                                          (assoc children filename child-inode))
                     :next-inode (inc child-inode))))))))
  (link [this inode filename target-inode continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              attr-table (:attr-table state)
              children (get lookup-table inode)
              dir-attr (get attr-table inode)
              attr (get attr-table target-inode)]
          (if (contains? children filename)
            (do (continuation! errno-exist) state)
            (if (not (= stat-type-directory
                        (bit-and stat-type-mask (:mode dir-attr))))
              (do (continuation! errno-notdir) state)
              (if (nil? attr)
                (do (continuation! errno-noent) state)
                (do
                  (agent-do state-agent
                            (continuation! (assoc attr :inode target-inode)))
                  (assoc state
                         :attr-table (assoc attr-table target-inode
                                            (assoc attr :nlink
                                                   (inc (:nlink attr))))
                         :lookup-table (assoc lookup-table inode
                                              (assoc children filename
                                                     target-inode)))))))))))
  (unlink [this inode filename continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              attr-table (:attr-table state)
              children (get lookup-table inode)
              child-inode (get children filename)
              dir-attr (get attr-table inode)]
          (if (not (= stat-type-directory
                      (bit-and stat-type-mask (:mode dir-attr))))
            (do (continuation! errno-notdir) state)
            (if (nil? child-inode)
              (do (continuation! errno-noent) state)
              (let [child-attr (get attr-table child-inode)]
                (if (= stat-type-directory
                       (bit-and stat-type-mask (:mode child-attr)))
                  (do (continuation! errno-isdir) state)
                  (let [nlink (dec (:nlink child-attr))]
                    (continuation! 0)
                    (assoc state
                           :lookup-table (assoc lookup-table inode
                                                (dissoc children filename))
                           :attr-table (if (zero? nlink)
                                         (dissoc attr-table child-inode)
                                         (assoc
                                           attr-table child-inode
                                           (assoc child-attr
                                                  :nlink nlink)))))))))))))
  (rmdir [this inode filename continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              attr-table (:attr-table state)
              children (get lookup-table inode)
              child-inode (get children filename)
              dir-attr (get attr-table inode)]
          (if (not (= stat-type-directory
                      (bit-and stat-type-mask (:mode dir-attr))))
            (do (continuation! errno-notdir) state)
            (if (nil? child-inode)
              (do (continuation! errno-noent) state)
              (let [child-attr (get attr-table child-inode)
                    child-children (get lookup-table child-inode)]
                (if (not (= stat-type-directory
                            (bit-and stat-type-mask (:mode child-attr))))
                  (do (continuation! errno-notdir) state)
                  (if (not (empty? (dissoc child-children "." "..")))
                    (do (continuation! errno-notempty) state)
                    (let [nlink (- (:nlink child-attr) 3)]
                      (agent-do state-agent (continuation! 0))
                      (assoc state
                             :lookup-table (dissoc
                                             (assoc lookup-table inode
                                                    (dissoc children filename))
                                             child-inode)
                             :attr-table (if (zero? nlink)
                                           (dissoc attr-table child-inode)
                                           (assoc
                                             attr-table child-inode
                                             (assoc child-attr
                                                    :nlink nlink))))))))))))))
  (chmod [this inode mode continuation!]
    (attr-attribute-modifier! state-agent inode
                              #(bit-or (bit-and stat-type-mask %)
                                       (bit-and stat-mode-mask mode))
                              :mode continuation!))
  (setuid [this inode uid continuation!]
    (attr-attribute-modifier! state-agent inode
                              (fn [_] uid)
                              :uid continuation!))
  (setgid [this inode gid continuation!]
    (attr-attribute-modifier! state-agent inode
                              (fn [_] gid)
                              :gid continuation!))
  (truncate [this inode size continuation!]
    ;; TODO
    (continuation! errno-nosys))
  (setatime [this inode seconds nseconds continuation!]
    (attr-modifier! state-agent inode
                    (fn [attr]
                      (assoc attr :atime seconds :atimensec nseconds))
                    continuation!))
  (setmtime [this inode seconds nseconds continuation!]
    (attr-modifier! state-agent inode
                    (fn [attr]
                      (assoc attr :mtime seconds :mtimensec nseconds))
                    continuation!)))
