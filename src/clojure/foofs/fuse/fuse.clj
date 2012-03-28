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

(defn start-filesystem
  [filesystem fd]
  (let [fuse {:filesystem filesystem
              :fd fd
              :read-thread (atom nil)
              :connection {:proto-major (atom 0)
                           :proto-minor (atom 0)}}
        read-thread (Thread. (partial read-loop! fuse))]
    (reset! (:read-thread fuse) read-thread)
    (.start read-thread)
    fuse))

(defn freebsd-mount
  [filesystem mountpoint]
  (try
    (let [fd (c-open "/dev/fuse" 0100002 0)
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
            (start-filesystem filesystem fd)
            (c-close fd)))
        (c-close fd)))
    (catch Exception e
      (.printStackTrace e)
      nil)))

(defn linux-mount
  [filesystem mountpoint]
  (try
    (let [sv (Memory. 8)]
      (socketpair pf-unix sock-stream 0 sv)
      (let [sock0 (.getInt sv 0)
            sock1 (.getInt sv 4)
            _ (.println *err* (str "socket pair " sock0 " " sock1))
            pid (IntByReference.)
            ret (posix_spawnp pid "fusermount" nil nil
                             ["fusermount" "--" mountpoint]
                             [(str "_FUSE_COMMFD=" sock0)])]
        (.println *err* (str "spawn " ret))
        (when (== 0 ret)
          (let [rv (receive-fd sock1)]
            (.println *err* (str "got fd " rv))
            (start-filesystem filesystem rv)))))
    (catch Exception e
      (.printStackTrace e)
      nil)))
