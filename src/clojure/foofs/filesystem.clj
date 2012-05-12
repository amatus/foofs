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
  [attr]
  {:nodeid (:inode attr)
   :generation 0
   :entry-valid 0
   :attr-valid 0
   :entry-valid-nsec 0
   :attr-valid-nsec 0
   :attr attr})

;; TODO - make use of argument destructuring
(defrecord FooFilesystem
  [^foofs.filesystembackend.FilesystemBackend backend
   ^clojure.lang.Agent readdir-agent]
  Filesystem
  (lookup [this request continuation!]
    (.lookup
      backend (:nodeid request) (:arg request)
      (fn [inode]
        (if (nil? inode)
          (continuation! errno-noent)
          (.getattr
            backend inode
            (fn [attr]
              (if (nil? attr)
                (continuation! errno-noent)
                (continuation! (fill-entry attr)))))))))
  (forget [this request]
    nil)
  (getattr [this request continuation!]
    (.getattr
      backend (:nodeid request)
      (fn [attr]
        (if (nil? attr)
          (continuation! errno-noent)
          (continuation! (assoc attr :valid 0 :valid-nsec 0))))))
  (setattr [this request continuation!]
    (let [nodeid (:nodeid request)
          arg (:arg request)
          valid (:valid arg)
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
  (mknod [this request continuation!]
    (let [arg (:arg request)]
      (.mknod
        backend (:nodeid request) (:filename arg) (:mode arg)
        (fn [attr]
          (if (integer? attr)
            (continuation! attr)
            (continuation! (fill-entry attr)))))))
  (mkdir [this request continuation!]
    (let [arg (:arg request)]
      (.mknod
        backend (:nodeid request) (:filename arg)
        (bit-or stat-type-directory (:mode arg))
        (fn [attr]
          (if (integer? attr)
            (continuation! attr)
            (let [inode (:inode attr)]
              ;; do we need to wait for these to finish?
              (.link backend inode "." inode skip)
              (.link backend inode ".." (:nodeid request) skip)
              (continuation! (fill-entry attr))))))))
  (unlink [this request continuation!]
    (.unlink backend (:nodeid request) (:arg request) continuation!))
  (rmdir [this request continuation!]
    (.rmdir backend (:nodeid request) (:arg request) continuation!))
  (link [this request continuation!]
    (let [arg (:arg request)]
      (.link
        backend (:nodeid request) (:filename arg) (:target-inode arg)
        (fn [attr]
          (if (integer? attr)
            (continuation! attr)
            (continuation! (fill-entry attr)))))))
  (open [this request continuation!]
    (.reference
      backend (:nodeid request)
      (fn [link]
        (if (integer? link)
          (continuation! link)
          (continuation! {:handle 0
                          :flags 0})))))
  (readfile [this request continuation!]
    (let [arg (:arg request)
          offset (:offset arg)
          size (:size arg)]
      (.readfile
        backend (:nodeid request) offset size
        (fn [buffer]
          (if (nil? buffer)
            (continuation! errno-noent)
            (continuation! buffer))))))
  (statfs [this request continuation!]
    (continuation!
      {:blocks 0
       :bfree 0
       :bavail 0
       :files 0
       :ffree 0
       :bsize 512
       :namelen 255
       :frsize 0}))
  (release [this request continuation!]
    (.dereference backend (:nodeid request) continuation!))
  (init [this request]
    (.println *err* "init called")
    (send
      readdir-agent
      (partial conj {:opendirs {}
                     :next-handle 0})))
  (opendir [this request continuation!]
    (.reference
      backend (:nodeid request)
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
  (readdir [this request continuation!]
    (let [arg (:arg request)
          handle (:handle arg)
          offset (:offset arg)
          size (:size arg)]
      (if (zero? offset) ;; if we're reading from zero refresh the dirents
        (.clonedir
          backend (:nodeid request)
          (fn [dirents]
            (let [encoded-dirents (encode-dirents dirents)]
              (send
                readdir-agent
                (fn [state]
                  (agent-do readdir-agent
                            (continuation! (take size encoded-dirents)))
                  (assoc-deep state encoded-dirents :opendirs handle))))))
        (let [dirents ((:opendirs (deref readdir-agent)) handle)]
          (continuation! (take size (drop offset dirents)))))))
  (releasedir [this request continuation!]
    (.dereference
      backend (:nodeid request)
      (fn [link]
        (continuation! link)
        (send
          readdir-agent
          (fn [state]
            (let [opendirs (:opendirs state)]
              (assoc state :opendirs
                     (dissoc opendirs (:handle (:arg request))))))))))
  (create [this request continuation!]
    ;; is create supposed to be atomic?
    (let [arg (:arg request)]
      (.mknod
        backend (:nodeid request) (:filename arg) (:mode arg)
        (fn [attr]
          (if (integer? attr)
            (continuation! attr)
            (.reference
              backend (:inode attr)
              (fn [link]
                (if (integer? link)
                  (continuation! link)
                  (continuation!
                    (assoc (fill-entry attr)
                           :handle 0
                           :flags 0))))))))))
  (destroy [this request]
    nil))
