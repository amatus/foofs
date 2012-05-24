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
        [foofs.fuse bytebuffer jna]
        foofs.util))

(def empty-attrs
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

(defn attrs-modifier!
  [state-agent inode f continuation!]
  (send
    state-agent
    (fn [state]
      (let [attrs-table (:attrs-table state)
            attrs (get attrs-table inode)]
        (if (nil? attrs)
          (do (continuation! errno-noent) state)
          (let [new-attrs (f attrs)]
            (agent-do state-agent (continuation! new-attrs))
            (assoc-deep state new-attrs :attrs-table inode)))))))

(defn attribute-modifier!
  [state-agent inode f attribute continuation!]
  (attrs-modifier! state-agent inode
                   (fn [attrs]
                     (assoc attrs attribute (f (get attrs attribute))))
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
    (let [attrs-table (:attrs-table (deref state-agent))
          attrs (get attrs-table inode)]
      (if (nil? attrs)
        (continuation! nil)
        (continuation! (assoc attrs :inode inode)))))
  (reference [this inode continuation!]
    (attribute-modifier! state-agent inode inc :nlink continuation!))
  (dereference [this inode continuation!]
    (attribute-modifier! state-agent inode dec :nlink continuation!))
  (clonedir [this inode continuation!]
    (let [state (deref state-agent)
          lookup-table (:lookup-table state)
          children (get lookup-table inode)
          attrs-table (:attrs-table state)]
      (continuation!
        (map
          (fn [kv]
            (let [[name inode] kv
                  attrs (get attrs-table inode)]
              {:name name
               :nodeid inode
               :type (:mode attrs)}))
          children))))
  (readfile [this inode offset size continuation!]
    (let [state (deref state-agent)
          file (get (:file-table state) inode)]
      (if (nil? file)
        (continuation! nil)
        (continuation! (take size (drop offset file))))))
  (writefile [this inode offset size data continuation!]
    (send
      state-agent
      (fn [state]
        (let [attrs-table (:attrs-table state)
              file-table (:file-table state)
              attrs (get attrs-table inode)
              file (get file-table inode)]
          (if (nil? attrs)
            (do (continuation! errno-noent) state)
            (let [tail-offset (+ offset size)
                  tail-size (- (max tail-offset (:size attrs)) tail-offset)
                  file-extended (concat file (repeat (byte 0)))
                  file-written (concat
                                 (take offset file-extended)
                                 (buffer-seq! data)
                                 (take tail-size
                                       (drop tail-offset file-extended)))
                  new-size (count file-written)]
              (agent-do state-agent
                        (continuation! {:size (.limit data)}))
              (assoc state
                     :attrs-table (assoc-deep attrs-table new-size inode :size)
                     :file-table (assoc file-table inode file-written))))))))
  (mknod [this inode filename mode continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              children (get lookup-table inode)
              attrs-table (:attrs-table state)]
          (if (contains? children filename)
            (do
              (continuation! errno-exist)
              state)
            (let [child-inode (next-key attrs-table (:next-inode state)
                                        Long/MIN_VALUE Long/MAX_VALUE)
                  attrs (conj empty-attrs
                              {:mode mode
                               :nlink 1})]
              (agent-do state-agent
                        (continuation! (assoc attrs :inode child-inode)))
              (assoc state
                     :attrs-table (assoc attrs-table child-inode attrs)
                     :lookup-table (assoc lookup-table inode
                                          (assoc children filename child-inode))
                     :next-inode (inc child-inode))))))))
  (link [this inode filename target-inode continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              attrs-table (:attrs-table state)
              children (get lookup-table inode)
              dir-attrs (get attrs-table inode)
              attrs (get attrs-table target-inode)]
          (if (contains? children filename)
            (do (continuation! errno-exist) state)
            (if (not (= stat-type-directory
                        (bit-and stat-type-mask (:mode dir-attrs))))
              (do (continuation! errno-notdir) state)
              (if (nil? attrs)
                (do (continuation! errno-noent) state)
                (do
                  (agent-do state-agent
                            (continuation! (assoc attrs :inode target-inode)))
                  (assoc state
                         :attrs-table (assoc attrs-table target-inode
                                             (assoc attrs :nlink
                                                    (inc (:nlink attrs))))
                         :lookup-table (assoc lookup-table inode
                                              (assoc children filename
                                                     target-inode)))))))))))
  (unlink [this inode filename continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              attrs-table (:attrs-table state)
              children (get lookup-table inode)
              child-inode (get children filename)
              dir-attrs (get attrs-table inode)]
          (if (not (= stat-type-directory
                      (bit-and stat-type-mask (:mode dir-attrs))))
            (do (continuation! errno-notdir) state)
            (if (nil? child-inode)
              (do (continuation! errno-noent) state)
              (let [child-attrs (get attrs-table child-inode)]
                (if (= stat-type-directory
                       (bit-and stat-type-mask (:mode child-attrs)))
                  (do (continuation! errno-isdir) state)
                  (let [nlink (dec (:nlink child-attrs))]
                    (continuation! 0)
                    (assoc state
                           :lookup-table (assoc lookup-table inode
                                                (dissoc children filename))
                           :attrs-table (if (zero? nlink)
                                          (dissoc attrs-table child-inode)
                                          (assoc
                                            attrs-table child-inode
                                            (assoc child-attrs
                                                   :nlink nlink)))))))))))))
  (rmdir [this inode filename continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              attrs-table (:attrs-table state)
              children (get lookup-table inode)
              child-inode (get children filename)
              dir-attrs (get attrs-table inode)]
          (if (not (= stat-type-directory
                      (bit-and stat-type-mask (:mode dir-attrs))))
            (do (continuation! errno-notdir) state)
            (if (nil? child-inode)
              (do (continuation! errno-noent) state)
              (let [child-attrs (get attrs-table child-inode)
                    child-children (get lookup-table child-inode)]
                (if (not (= stat-type-directory
                            (bit-and stat-type-mask (:mode child-attrs))))
                  (do (continuation! errno-notdir) state)
                  (if (not (empty? (dissoc child-children "." "..")))
                    (do (continuation! errno-notempty) state)
                    ;; TODO: 3 is wrong if . or .. don't exist
                    (let [nlink (- (:nlink child-attrs) 3)]
                      (agent-do state-agent (continuation! 0))
                      (assoc state
                             :lookup-table (dissoc
                                             (assoc lookup-table inode
                                                    (dissoc children filename))
                                             child-inode)
                             :attrs-table (if (zero? nlink)
                                            (dissoc attrs-table child-inode)
                                            (assoc
                                              attrs-table child-inode
                                              (assoc child-attrs
                                                     :nlink nlink))))))))))))))
  (chmod [this inode mode continuation!]
    (attribute-modifier! state-agent inode
                         #(bit-or (bit-and stat-type-mask %)
                                  (bit-and stat-mode-mask mode))
                         :mode continuation!))
  (setuid [this inode uid continuation!]
    (attribute-modifier! state-agent inode
                         (fn [_] uid)
                         :uid continuation!))
  (setgid [this inode gid continuation!]
    (attribute-modifier! state-agent inode
                         (fn [_] gid)
                         :gid continuation!))
  (truncate [this inode size continuation!]
    (send
      state-agent
      (fn [state]
        (let [attrs-table (:attrs-table state)
              file-table (:file-table state)
              attrs (get attrs-table inode)
              file (get file-table inode)]
          (if (nil? attrs)
            (do (continuation! errno-noent) state)
            (do
              (agent-do state-agent (continuation! nil))
              (assoc
                state
                :attrs-table (assoc-deep attrs-table size inode :size)
                :file-table (assoc
                              file-table inode
                              (take size
                                    (concat file (repeat (byte 0))))))))))))
  (setatime [this inode seconds nseconds continuation!]
    (attrs-modifier! state-agent inode
                     (fn [attrs]
                       (assoc attrs :atime seconds :atimensec nseconds))
                     continuation!))
  (setmtime [this inode seconds nseconds continuation!]
    (attrs-modifier! state-agent inode
                     (fn [attrs]
                       (assoc attrs :mtime seconds :mtimensec nseconds))
                     continuation!))
  (rename [this inode target-inode filename target-filename continuation!]
    (continuation! errno-nosys)))
