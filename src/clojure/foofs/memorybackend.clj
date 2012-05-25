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

(def empty-inode
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

(defn inode-modifier!
  [state-agent nodeid f continuation!]
  (send
    state-agent
    (fn [state]
      (let [inode-table (:inode-table state)
            inode (get inode-table nodeid)]
        (if (nil? inode)
          (do (continuation! errno-noent) state)
          (let [new-inode (f inode)]
            (agent-do state-agent (continuation! new-inode))
            (assoc-deep state new-inode :inode-table nodeid)))))))

(defn attribute-modifier!
  [state-agent nodeid f attribute continuation!]
  (inode-modifier! state-agent nodeid
                   (fn [inode]
                     (assoc inode attribute (f (get inode attribute))))
                   continuation!))

(defrecord MemoryBackend
  [^clojure.lang.Agent state-agent]
  FilesystemBackend
  (lookup [_ nodeid child continuation!]
    (let [lookup-table (:lookup-table (deref state-agent))]
      (if (= "" child)
        (continuation! nodeid)
        (continuation! (get-in lookup-table [nodeid child])))))
  (getattr [_ nodeid continuation!]
    (let [inode (get-in (deref state-agent) [:inode-table nodeid])]
      (if (nil? inode)
        (continuation! nil)
        (continuation! (assoc inode :nodeid nodeid)))))
  (reference [_ nodeid continuation!]
    (attribute-modifier! state-agent nodeid inc :nlink continuation!))
  (dereference [_ nodeid continuation!]
    (attribute-modifier! state-agent nodeid dec :nlink continuation!))
  (clonedir [_ nodeid continuation!]
    (let [state (deref state-agent)
          inode-table (:inode-table state)]
      (continuation!
        (map
          (fn [[filename child-nodeid]]
            {:name filename
             :nodeid child-nodeid
             :type (get-in inode-table [child-nodeid :mode])})
          (get-in state [:lookup-table nodeid])))))
  (readfile [_ nodeid offset size continuation!]
    (let [file (get-in (deref state-agent) [:file-table nodeid])]
      (if (nil? file)
        (continuation! nil)
        (continuation! (take size (drop offset file))))))
  (writefile [_ nodeid offset size data continuation!]
    (send
      state-agent
      (fn [state]
        (let [inode-table (:inode-table state)
              file-table (:file-table state)
              inode (get inode-table nodeid)
              file (get file-table nodeid)]
          (if (nil? inode)
            (do (continuation! errno-noent) state)
            (let [tail-offset (+ offset size)
                  tail-size (- (max tail-offset (:size inode)) tail-offset)
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
                     :inode-table (assoc-deep inode-table new-size nodeid :size)
                     :file-table (assoc file-table nodeid file-written))))))))
  (mknod [_ nodeid filename mode continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              children (get lookup-table nodeid)
              inode-table (:inode-table state)]
          (if (contains? children filename)
            (do (continuation! errno-exist) state)
            (let [child-nodeid (next-key inode-table (:next-nodeid state)
                                         Long/MIN_VALUE Long/MAX_VALUE)
                  inode (conj empty-inode
                              {:mode mode
                               :nlink 1})]
              (agent-do state-agent
                        (continuation! (assoc inode :nodeid child-nodeid)))
              (assoc state
                     :inode-table (assoc inode-table child-nodeid inode)
                     :lookup-table (assoc lookup-table nodeid
                                          (assoc children filename
                                                 child-nodeid))
                     :next-nodeid (inc child-nodeid))))))))
  (link [_ nodeid filename target-nodeid continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              inode-table (:inode-table state)
              children (get lookup-table nodeid)
              inode (get inode-table nodeid)
              target-inode (get inode-table target-nodeid)]
          (if (contains? children filename)
            (do (continuation! errno-exist) state)
            (if (not (= stat-type-directory
                        (bit-and stat-type-mask (:mode inode))))
              (do (continuation! errno-notdir) state)
              (if (nil? target-inode)
                (do (continuation! errno-noent) state)
                (do
                  (agent-do state-agent
                            (continuation! (assoc target-inode
                                                  :nodeid target-nodeid)))
                  (assoc state
                         :inode-table (assoc inode-table target-nodeid
                                             (assoc target-inode :nlink
                                                    (inc
                                                      (:nlink target-inode))))
                         :lookup-table (assoc lookup-table nodeid
                                              (assoc children filename
                                                     target-nodeid)))))))))))
  (unlink [_ nodeid filename continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              inode-table (:inode-table state)
              children (get lookup-table nodeid)
              child-nodeid (get children filename)
              inode (get inode-table nodeid)]
          (if (not (= stat-type-directory
                      (bit-and stat-type-mask (:mode inode))))
            (do (continuation! errno-notdir) state)
            (if (nil? child-nodeid)
              (do (continuation! errno-noent) state)
              (let [child-inode (get inode-table child-nodeid)]
                (if (= stat-type-directory
                       (bit-and stat-type-mask (:mode child-inode)))
                  (do (continuation! errno-isdir) state)
                  (let [nlink (dec (:nlink child-inode))]
                    (continuation! 0)
                    (assoc state
                           :lookup-table (assoc lookup-table nodeid
                                                (dissoc children filename))
                           :inode-table (if (zero? nlink)
                                          (dissoc inode-table child-nodeid)
                                          (assoc
                                            inode-table child-nodeid
                                            (assoc child-inode
                                                   :nlink nlink)))))))))))))
  (rmdir [_ nodeid filename continuation!]
    (send
      state-agent
      (fn [state]
        (let [lookup-table (:lookup-table state)
              inode-table (:inode-table state)
              children (get lookup-table nodeid)
              child-nodeid (get children filename)
              inode (get inode-table nodeid)]
          (if (not (= stat-type-directory
                      (bit-and stat-type-mask (:mode inode))))
            (do (continuation! errno-notdir) state)
            (if (nil? child-nodeid)
              (do (continuation! errno-noent) state)
              (let [child-inode (get inode-table child-nodeid)
                    child-children (get lookup-table child-nodeid)]
                (if (not (= stat-type-directory
                            (bit-and stat-type-mask (:mode child-inode))))
                  (do (continuation! errno-notdir) state)
                  (if (not (empty? (dissoc child-children "." "..")))
                    (do (continuation! errno-notempty) state)
                    ;; TODO: 3 is wrong if . or .. don't exist
                    (let [nlink (- (:nlink child-inode) 3)]
                      (agent-do state-agent (continuation! 0))
                      (assoc state
                             :lookup-table (dissoc
                                             (assoc lookup-table nodeid
                                                    (dissoc children filename))
                                             child-nodeid)
                             :inode-table (if (zero? nlink)
                                            (dissoc inode-table child-nodeid)
                                            (assoc
                                              inode-table child-nodeid
                                              (assoc child-inode
                                                     :nlink nlink))))))))))))))
  (chmod [_ nodeid mode continuation!]
    (attribute-modifier! state-agent nodeid
                         #(bit-or (bit-and stat-type-mask %)
                                  (bit-and stat-mode-mask mode))
                         :mode continuation!))
  (setuid [_ nodeid uid continuation!]
    (attribute-modifier! state-agent nodeid
                         (fn [_] uid)
                         :uid continuation!))
  (setgid [_ nodeid gid continuation!]
    (attribute-modifier! state-agent nodeid
                         (fn [_] gid)
                         :gid continuation!))
  (truncate [_ nodeid size continuation!]
    (send
      state-agent
      (fn [state]
        (let [inode-table (:inode-table state)
              file-table (:file-table state)
              file (get file-table nodeid)]
          (if (contains? inode-table nodeid)
            (do
              (agent-do state-agent (continuation! nil))
              (assoc
                state
                :inode-table (assoc-deep inode-table size nodeid :size)
                :file-table (assoc
                              file-table nodeid
                              (take size
                                    (concat file (repeat (byte 0)))))))
            (do (continuation! errno-noent) state))))))
  (setatime [_ nodeid seconds nseconds continuation!]
    (inode-modifier! state-agent nodeid
                     #(assoc % :atime seconds :atimensec nseconds)
                     continuation!))
  (setmtime [_ nodeid seconds nseconds continuation!]
    (inode-modifier! state-agent nodeid
                     #(assoc % :mtime seconds :mtimensec nseconds)
                     continuation!))
  (rename [_ nodeid target-nodeid filename target-filename continuation!]
    (continuation! errno-nosys)))
