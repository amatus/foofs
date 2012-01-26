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

(ns foofs.fuse.fuse
  (:use (foofs.fuse jna protocol))
  (:import com.sun.jna.Memory
           com.sun.jna.ptr.IntByReference))

(defn read-loop!
  [fuse]
  (let [fd (:fd fuse)]
    (try
      (loop []
        (let [mem (Memory. 0x21000)
              ret (c-read fd mem (.size mem))] 
          (process-buf! fuse (.getByteBuffer mem 0 ret)))
          (recur))
      (catch Exception e
        (.printStackTrace e)))))

(defprotocol Filesystem
  "A FUSE filesystem."
  (getattr [this request continuation!] "Get file attributes.")
  (statfs [this request continuation!] "Get file system statistics.")
  (init [this request] "Initialize filesystem.")
  (opendir [this request continuation!] "Open directory.")
  (readdir [this request continuation!] "Read directory.")
  (releasedir [this request continuation!] "Release Directory.")
  ;; and so on
  )

(def test-fs
  (reify Filesystem
    (getattr [this request continuation!]
      (continuation!
        {:inode 0
         :size 0
         :blocks 0
         :atime 0
         :mtime 0
         :ctime 0
         :atimensec 0
         :mtimensec 0
         :ctimensec 0
         :mode 040000
         :nlink 1
         :uid 0
         :gid 0
         :rdev 0}))
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
    (init [this request]
      (.println *err* "init called."))
    (opendir [this request continuation!]
      (continuation!
        {:handle 1
         :flags 0}))
    (readdir [this request continuation!]
      (continuation!
        []))
    (releasedir [this request continuation!]
      (continuation! nil))))

(defn freebsd-mount
  [filesystem mountpoint]
  (try
    (let [fd (open "/dev/fuse" 0100002 0)
          pid (IntByReference.)
          ret (posix_spawn pid "/usr/sbin/mount_fusefs" nil nil
                           ["mount_fusefs" "-o" "default_permissions" (str fd)
                            mountpoint]
                           ["MOUNT_FUSEFS_SAFE=1"
                            "MOUNT_FUSEFS_CALL_BY_LIB=1"])]
      (if (== 0 ret)
        (let [stat_loc (IntByReference.)]
          (waitpid (.getValue pid) stat_loc 0)
          (if (== 0 (.getValue stat_loc))
            (let [fuse {:filesystem filesystem
                        :fd fd
                        :read-thread (atom nil)
                        :connection {:proto-major (atom 0)
                                     :proto-minor (atom 0)}}
                  read-thread (Thread. (partial read-loop! fuse))]
              (reset! (:read-thread fuse) read-thread)
              (.start read-thread)
              fuse)
            (close fd)))
        (close fd)))
    (catch Exception e
      (.printStackTrace e)
      nil)))

