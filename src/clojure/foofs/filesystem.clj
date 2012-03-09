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
        (foofs.fuse jna protocol)))

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
                (continuation!
                  {:nodeid inode
                   :generation 0
                   :entry-valid 0
                   :attr-valid 0
                   :entry-valid-nsec 0
                   :attr-valid-nsec 0
                   :attr attr}))))))))
  (forget [this request]
    nil)
  (getattr [this request continuation!]
    (.getattr
      backend (:nodeid request)
      (fn [attr]
        (if (nil? attr)
          (continuation! errno-noent)
          (continuation! attr)))))
  (open [this request continuation!]
    (.reference
      backend (:nodeid request)
      (fn [link]
        (if (nil? link)
          (continuation! errno-noent)
          (continuation! {:handle 0
                          :flags 0})))))
  (readfile [this request continuation!]
    (continuation! []))
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
    (.dereference
      backend (:nodeid request)
      (fn [link]
        (if (nil? link)
          (continuation! errno-noent)
          (continuation! nil)))))
  (init [this request]
    (send
      readdir-agent
      (partial conj {:opendirs {}
                     :next-handle 0})))
  (opendir [this request continuation!]
    (.reference
      backend (:nodeid request)
      (fn [link]
        (if (nil? link)
          (continuation! errno-noent)
          (send
            readdir-agent
            (fn [state]
              (let [opendirs (:opendirs state)
                    next-handle (:next-handle state)
                    handle (first
                             (remove
                               (partial contains? opendirs)
                               (concat
                                 (range next-handle Long/MAX_VALUE)
                                 (range Long/MIN_VALUE next-handle))))]
                (send
                  readdir-agent
                  (fn [state]
                    (continuation! {:handle handle
                                    :flags 0})
                    state))
                {:opendirs (assoc opendirs handle [])
                 :next-handle (inc handle)})))))))
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
                  (send
                    readdir-agent
                    (fn [state]
                      (continuation! (take size encoded-dirents))
                      state))
                  {:opendirs (assoc (:opendirs state) handle encoded-dirents)
                   :next-handle (:next-handle state)})))))
        (let [dirents ((:opendirs (deref readdir-agent)) handle)]
          (continuation! (take size (drop offset dirents)))))))
  (releasedir [this request continuation!]
    (.dereference
      backend (:nodeid request)
      (fn [link]
        (if (nil? link)
          (continuation! errno-noent)
          (continuation! nil))
        (send
          readdir-agent
          (fn [state]
            {:opendirs (dissoc (:opendirs state) (:handle (:arg request)))
             :next-handle (:next-handle state)})))))
  (destroy [this request]
    nil))