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
  (getattr [this request] "Get file attributes.")
  (statfs [this request] "Get file system statistics.")
  (init [this request] "Initialize filesystem.")
  (opendir [this request flags] "Open directory.")
  ;; and so on
  )

(def test-fs
  (reify Filesystem
    (getattr [this request]
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
       :rdev 0})
    (statfs [this request]
      {:blocks 0
       :bfree 0
       :bavail 0
       :files 0
       :ffree 0
       :bsize 512
       :namelen 255
       :frsize 0})
    (init [this request]
      (.write *out* "init called.\n"))
    (opendir [this request flags]
      {:handle 1
       :flags 0})))

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

