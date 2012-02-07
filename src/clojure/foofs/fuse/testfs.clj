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

(ns foofs.fuse.testfs
  (:use (foofs.fuse fuse jna protocol)))

(def default-entry
  {:generation 0
   :entry-valid 0
   :attr-valid 0
   :entry-valid-nsec 0
   :attr-valid-nsec 0})

(def default-attr
  {:inode 0
   :size 0
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

(def hello-world-bytes (.getBytes "hello world!\n" "UTF-8"))

(def inodes
  {1 {:attr (conj
              default-attr
              {:inode 1
               :mode stat-type-directory
               :nlink 1})
      :dirents {"." {:name "."
                     :nodeid 1
                     :type stat-type-directory}
                ".." {:name ".."
                      :nodeid 1
                      :type stat-type-directory}
                "test" {:name "test"
                        :nodeid 2
                        :type stat-type-regular}
                "folder" {:name "folder"
                          :nodeid 3
                          :type stat-type-directory}}}
   2 {:attr (conj
              default-attr
              {:inode 2
               :size (count hello-world-bytes)
               :blocks 1
               :mode stat-type-regular
               :nlink 1})
      :content hello-world-bytes}
   3 {:attr (conj
              default-attr
              {:inode 3
               :mode stat-type-directory
               :nlink 1})
      :dirents {"." {:name "."
                     :nodeid 3
                     :type stat-type-directory}
                ".." {:name ".."
                      :nodeid 1
                      :type stat-type-directory}
                     }}
   }
  )

(defrecord TestFS
  [^clojure.lang.Agent state-agent]
  Filesystem
  (lookup [this request continuation!]
    (.println *err* (str "lookup: " request))
    (if (not (contains? inodes (:nodeid request)))
      (continuation! errno-noent)
      (let [inode (inodes (:nodeid request))]
        (if (= "" (:arg request))
          (continuation!
            (conj
              default-entry
              {:nodeid (:nodeid request)
               :attr (:attr inode)}))
          (if (not (contains? (:dirents inode) (:arg request)))
            (continuation! errno-noent)
            (let [dirent ((:dirents inode) (:arg request))
                  nodeid (:nodeid dirent)
                  inode (inodes nodeid)]
              (continuation!
                (conj
                  default-entry
                  {:nodeid nodeid
                   :attr (:attr inode)}))))))))
  (forget [this request]
    (.println *err* (str "forget: " request)))
  (getattr [this request continuation!]
    (.println *err* (str "getattr: " request))
    (if (not (contains? inodes (:nodeid request)))
      (continuation! errno-noent)
      (continuation! (:attr (inodes (:nodeid request))))))
  (open [this request continuation!]
    (.println *err* (str "open: " request))
    (if (not (contains? inodes (:nodeid request)))
      (continuation! errno-noent)
      (send (:state-agent this)
            (fn [state]
              (let [handle (:next-handle state)]
                ;; do another send to make sure state is updated before
                ;; continuation! is called
                (send (:state-agent this)
                      (fn [state]
                        (continuation! {:handle handle :flags 0})
                        state))
                (conj state {:handles (assoc
                                        (:handles state)
                                        handle
                                        (:content (inodes (:nodeid request))))
                             :next-handle (inc handle)}))))))
  (read [this request continuation!]
    (.println *err* (str "read: " request))
    (let [handles (:handles (deref (:state-agent this)))
          arg (:arg request)
          result (take (:size arg)
                       (drop (:offset arg)
                             (handles (:handle arg))))]
      (continuation! result)))
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
    (.println *err* (str "release: " request))
    (continuation! nil)
    (send (:state-agent this)
          (fn [state]
            (assoc state
                   :handles (dissoc (:handles state)
                                    (:handle (:arg request)))))))
  (init [this request]
    (.println *err* "init called.")
    (send (:state-agent this)
          (fn [state]
            {:handles {} :next-handle 0})))
  (opendir [this request continuation!]
    (.println *err* (str "opendir: " request))
    (if (not (contains? inodes (:nodeid request)))
      (continuation! errno-noent)
      (send (:state-agent this)
            (fn [state]
              (let [dirents (encode-dirents
                              (vals (:dirents (inodes (:nodeid request)))))
                    handle (:next-handle state)]
                ;; do another send to make sure state is updated before
                ;; continuation! is called
                (send (:state-agent this)
                      (fn [state]
                        (continuation! {:handle handle :flags 0})
                        state))
                (conj state {:handles (assoc (:handles state)
                                             handle
                                             dirents)
                             :next-handle (inc handle)}))))))
  (readdir [this request continuation!]
    (.println *err* (str "readdir: " request))
    (let [handles (:handles (deref (:state-agent this)))
          arg (:arg request)
          result (take (:size arg)
                       (drop (:offset arg)
                             (handles (:handle arg))))]
      (continuation! result)))
  (releasedir [this request continuation!]
    (.println *err* (str "releasedir: " request))
    (continuation! nil)
    (send (:state-agent this)
          (fn [state]
            (assoc state
                   :handles (dissoc (:handles state)
                                    (:handle (:arg request)))))))
  (destroy [this request]
    (.println *err* "destroy called.")))
