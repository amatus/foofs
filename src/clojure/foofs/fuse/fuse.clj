(ns foofs.fuse.fuse
  (:use (foofs.fuse jna protocol)))

(defn mem-seq
  [mem]
  (map #(.getByte mem %) (range 0 (.size mem))))

(defn read-loop!
  [filesystem fd]
  (try
    (let [buf (Memory. 0x21000)
          ret (c-read fd buf (.size buf))]
      (process-buf filesystem (.getByteBuffer buf 0 ret))
    (recur fd)
    (catch Exception e
      nil)))

(defprotocol Filesystem
  "A FUSE filesystem."
  (getattr [path] "Get file attributes.")
  (readlink [path] "Read the target of a symbolic link.")
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
              (.start read-thread)
              nil)
            (close fd)))
        (close fd)))
    (catch Exception e
      nil)))

