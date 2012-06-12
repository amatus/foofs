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
  (:use [foofs.filesystembackend :only [FilesystemBackend]]
        [foofs.fuse.filesystem :only [Filesystem]]
        foofs.util
        (foofs.fuse jna protocol)))

(defn fill-entry
  [inode]
  {:nodeid (:nodeid inode)
   :generation 0
   :entry-valid 0
   :attr-valid 0
   :entry-valid-nsec 0
   :attr-valid-nsec 0
   :attr inode})

;; TODO - the backend operations will eventually be interleaved with operations
;; from other clients. These need to be transformed to not operate on inode
;; numbers, because those will not be the same between clients. It seems like
;; a bad idea to call backend operations conditionally on the results of other
;; backend operations since they may return different results on different
;; clients.
(defrecord FooFilesystem
  [^foofs.filesystembackend.FilesystemBackend backend
   ^clojure.lang.Agent readdir-agent]
  Filesystem
  (lookup [_ {:keys [nodeid arg]} continuation!]
    (.lookup
      backend nodeid arg
      (fn [child-nodeid]
        (if (nil? child-nodeid)
          (continuation! errno-noent)
          (.getattr
            backend child-nodeid
            (fn [inode]
              (if (nil? inode)
                (continuation! errno-noent)
                (continuation! (fill-entry inode)))))))))
  (forget [_ _]
    nil)
  (getattr [_ {:keys [nodeid]} continuation!]
    (.getattr
      backend nodeid
      (fn [inode]
        (if (nil? inode)
          (continuation! errno-noent)
          (continuation! (assoc inode :valid 0 :valid-nsec 0))))))
  (setattr [this {:keys [nodeid arg] :as request} continuation!]
    (let [valid (:valid arg)
          set-mode! (fn [next!]
                     (if (bit-test valid setattr-valid-mode)
                       (.chmod
                         backend nodeid (:mode arg)
                         (fn [result]
                           (if (integer? result)
                             (continuation! result)
                             (next!))))
                       (next!)))
          set-uid! (fn [next!]
                     (if (bit-test valid setattr-valid-uid)
                       (.setuid
                         backend nodeid (:uid arg)
                         (fn [result]
                           (if (integer? result)
                             (continuation! result)
                             (next!))))
                       (next!)))
          set-gid! (fn [next!]
                     (if (bit-test valid setattr-valid-gid)
                       (.setgid
                         backend nodeid (:gid arg)
                         (fn [result]
                           (if (integer? result)
                             (continuation! result)
                             (next!))))
                       (next!)))
          truncate! (fn [next!]
                      (if (bit-test valid setattr-valid-size)
                        (.truncate
                          backend nodeid (:size arg)
                          (fn [result]
                            (if (integer? result)
                              (continuation! result)
                              (next!))))
                        (next!)))
          set-atime! (fn [next!]
                       (if (bit-test valid setattr-valid-atime)
                         (.setatime
                           backend nodeid (:atime arg) (:atimensec arg)
                           (fn [result]
                             (if (integer? result)
                               (continuation! result)
                               (next!))))
                         (next!)))
          set-mtime! (fn [next!]
                       (if (bit-test valid setattr-valid-mtime)
                         (.setmtime
                           backend nodeid (:mtime arg) (:mtimensec arg)
                           (fn [result]
                             (if (integer? result)
                               (continuation! result)
                               (next!))))
                         (next!)))]
      (chain
        set-mode!
        set-uid!
        set-gid!
        truncate!
        set-atime!
        set-mtime!
        #(.getattr this request continuation!))))
  (mknod [_ {:keys [nodeid arg]} continuation!]
    (.mknod
      backend nodeid (:filename arg) (:mode arg)
      (fn [inode]
        (if (integer? inode)
          (continuation! inode)
          (continuation! (fill-entry inode))))))
  (mkdir [_ {:keys [nodeid arg]} continuation!]
    (.mkdir
      backend nodeid (:filename arg) (:mode arg)
      (fn [inode]
        (if (integer? inode)
          (continuation! inode)
          (continuation! (fill-entry inode))))))
  (unlink [_ {:keys [nodeid arg]} continuation!]
    (.unlink backend nodeid arg continuation!))
  (rmdir [_ {:keys [nodeid arg]} continuation!]
    (.rmdir backend nodeid arg continuation!))
  (rename [_ {:keys [nodeid arg]} continuation!]
    (.rename backend nodeid (:target-nodeid arg) (:filename arg)
             (:target-filename arg) continuation!))
  (link [_ {:keys [nodeid arg]} continuation!]
    (.link
      backend nodeid (:filename arg) (:target-nodeid arg)
      (fn [inode]
        (if (integer? inode)
          (continuation! inode)
          (continuation! (fill-entry inode))))))
  (open [_ {:keys [nodeid]} continuation!]
    (.reference
      backend nodeid
      (fn [link]
        (if (integer? link)
          (continuation! link)
          (continuation! {:handle 0
                          :flags 0})))))
  (readfile [_ {:keys [nodeid arg]} continuation!]
    (.readfile backend nodeid (:offset arg) (:size arg) continuation!))
  (writefile [_ {:keys [nodeid arg]} continuation!]
    (.writefile
      backend nodeid (:offset arg) (:size arg) (:data arg) continuation!))
  (statfs [_ _ continuation!]
    (continuation!
      {:blocks 0
       :bfree 0
       :bavail 0
       :files 0
       :ffree 0
       :bsize 512
       :namelen 255
       :frsize 0}))
  (release [_ {:keys [nodeid]} continuation!]
    (.dereference backend nodeid continuation!))
  (init [_ _]
    (.println *err* "init called")
    (send
      readdir-agent
      (partial conj {:opendirs {}
                     :next-handle 0})))
  (opendir [_ {:keys [nodeid]} continuation!]
    (.reference
      backend nodeid
      (fn [link]
        (if (integer? link)
          (continuation! link)
          (send
            readdir-agent
            (fn [state]
              (let [opendirs (:opendirs state)
                    next-handle (:next-handle state)
                    handle (next-key opendirs next-handle Long/MIN_VALUE
                                     Long/MAX_VALUE)]
                (if (nil? handle)
                  (do (continuation! errno-nomem) state)
                  (do
                    (agent-do readdir-agent
                              (continuation! {:handle handle :flags 0}))
                    (assoc state
                           :opendirs (assoc opendirs handle nil)
                           :next-handle (inc handle)))))))))))
  (readdir [_ {:keys [nodeid arg]} continuation!]
    (let [handle (:handle arg)
          offset (:offset arg)
          size (:size arg)]
      (if (zero? offset) ;; if we're reading from zero refresh the dirents
        (.clonedir
          backend nodeid
          (fn [dirents]
            (let [encoded-dirents (encode-dirents dirents)]
              (send
                readdir-agent
                (fn [state]
                  (agent-do readdir-agent
                            (continuation! (take size encoded-dirents)))
                  (assoc-in state [:opendirs handle] encoded-dirents))))))
        (let [dirents ((:opendirs (deref readdir-agent)) handle)]
          (continuation! (take size (drop offset dirents)))))))
  (releasedir [_ {:keys [nodeid arg]} continuation!]
    (.dereference
      backend nodeid
      (fn [link]
        (continuation! link)
        (send
          readdir-agent
          (fn [state]
            (let [opendirs (:opendirs state)]
              (assoc state :opendirs
                     (dissoc opendirs (:handle arg)))))))))
  (create [_ {:keys [nodeid arg]} continuation!]
    ;; is create supposed to be atomic?
    (.mknod
      backend nodeid (:filename arg) (:mode arg)
      (fn [inode]
        (if (integer? inode)
          (continuation! inode)
          (.reference
            backend (:nodeid inode)
            (fn [link]
              (if (integer? link)
                (continuation! link)
                (continuation!
                  (assoc (fill-entry inode)
                         :handle 0
                         :flags 0)))))))))
  (destroy [_ _]
    nil))
