(ns foofs.fuse.fuse
  (:use (foofs.fuse jna protocol))
  (:import com.sun.jna.Memory
           com.sun.jna.ptr.IntByReference))

(defn mem-seq
  [mem]
  (map #(.getByte mem %) (range 0 (.size mem))))

(defn read-loop!
  [filesystem fd]
  (let [fuse {:filesystem filesystem
              :fd fd
              :connection {:proto-major (atom 0)
                           :proto-minor (atom 0)}}]
    (when
      (try
        (let [mem (Memory. 0x21000)
              ret (c-read fd mem (.size mem))] 
          (process-buf fuse (.getByteBuffer mem 0 ret)))
        true
        (catch Exception e
          (.printStackTrace e)
          false))
      (recur filesystem fd))))

(defprotocol Filesystem
  "A FUSE filesystem."
  ;;(getattr [self path] "Get file attributes.")
  ;;(readlink [self path] "Read the target of a symbolic link.")
  (init [self] "Initialize filesystem.")
  ;; and so on
  )

(defn freebsd-mount
  [filesystem mountpoint]
  (try
    (let [fd (open "/dev/fuse" 0100002 0)
          pid (IntByReference.)
          ret (posix_spawn pid "/usr/sbin/mount_fusefs" nil nil
                           ["mount_fusefs" (str fd) mountpoint]
                           ["MOUNT_FUSEFS_SAFE=1"
                            "MOUNT_FUSEFS_CALL_BY_LIB=1"])]
      (if (== 0 ret)
        (let [stat_loc (IntByReference.)]
          (waitpid (.getValue pid) stat_loc 0)
          (if (== 0 (.getValue stat_loc))
            (let [read-thread (Thread. (partial read-loop! filesystem fd))]
              (.write *out* "Starting read thread\n")
              (.start read-thread)
              nil)
            (close fd)))
        (close fd)))
    (catch Exception e
      (.printStackTrace e)
      nil)))

