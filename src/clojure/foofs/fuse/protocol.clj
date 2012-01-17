(ns foofs.fuse.protocol
  (:use (foofs.fuse bytebuffer jna parser)
        clojure.contrib.monads)
  (:import com.sun.jna.Memory))

(def fuse-version-major 7)
(def fuse-version-minor 8)
(def fuse-root-id 1)

(def op-lookup       1)
(def op-forget       2)
(def op-getattr      3)
(def op-setattr      4)
(def op-readlink     5)
(def op-symlink      6)
(def op-mknod        8)
(def op-mkdir        9)
(def op-unlink       10)
(def op-rmdir        11)
(def op-rename       12)
(def op-link         13)
(def op-open         14)
(def op-read         15)
(def op-write        16)
(def op-statfs       17)
(def op-release      18)
(def op-fsync        20)
(def op-setxattr     21)
(def op-getxattr     22)
(def op-listxattr    23)
(def op-removexattr  24)
(def op-flush        25)
(def op-init         26)
(def op-opendir      27)
(def op-readdir      28)
(def op-releasedir   29)
(def op-fsyncdir     30)
(def op-getlk        31)
(def op-setlk        32)
(def op-setlkw       33)
(def op-access       34)
(def op-create       35)
(def op-interrupt    36)
(def op-bmap         37)
(def op-destroy      38)
(def op-ioctl        39)
(def op-poll         40)
(def op-notify-reply 41)
(def op-batch-forget 42)

(defn write-out-header
  [out]
  (domonad state-m
    [_ (write-int32 (:len out))
     _ (write-int32 (:error out))
     _ (write-int64 (:unique out))]
    nil))

(def out-header-len 16)

(defn send-reply!
  [fuse request error reply]
  (let [mem (Memory. 0x21000)
        buf (.getByteBuffer mem 0 (.size mem))]
    (.clear buf)
    (.position buf out-header-len)
    (reply buf)
    (.flip buf)
    ((write-out-header {:len (.limit buf)
                        :error error
                        :unique (:unique request)})
       buf)
    (try
      (let [ret (c-write (:fd fuse) mem (.limit buf))]
        )
      (catch Exception e nil)))
  nil)

(defn reply-error!
  [fuse request error]
  (send-reply! fuse request error (with-monad state-m m-zero)))

(defn reply-ok!
  [fuse request reply]
  (send-reply! fuse request 0 reply))

(def parse-in-header
  (domonad
    parser-m
    [len parse-uint32
     opcode parse-opaque32
     unique parse-opaque64
     nodeid parse-opaque64
     uid parse-opaque32
     gid parse-opaque32
     pid parse-opaque32
     _ skip-32]
    {:len len
     :opcode opcode
     :unique unique
     :nodeid nodeid
     :uid uid
     :gid gid
     :pid pid}))

(defn write-fuse-attr
  [attr]
  (domonad
    state-m
    [_ (write-int64 (:inode attr))
     _ (write-int64 (:size attr))
     _ (write-int64 (:blocks attr))
     _ (write-int64 (:atime attr))
     _ (write-int64 (:mtime attr))
     _ (write-int64 (:ctime attr))
     _ (write-int32 (:atimensec attr))
     _ (write-int32 (:mtimensec attr))
     _ (write-int32 (:ctimensec attr))
     _ (write-int32 (:mode attr))
     _ (write-int32 (:nlink attr))
     _ (write-int32 (:uid attr))
     _ (write-int32 (:gid attr))
     _ (write-int32 (:rdev attr))]
    nil))

(defn write-attr-out
  [valid valid-nsec]
  (domonad
    state-m
    [_ (write-int64 valid)
     _ (write-int32 valid-nsec)
     _ (pad 4)]
    nil))

(defn process-getattr!
  [fuse request arg]
  (let [result (.getattr (:filesystem fuse) request)]
    (cond 
      (map? result) (reply-ok!
                      fuse
                      request
                      (domonad
                        state-m
                        [_ (write-attr-out 0 0)
                         _ (write-fuse-attr result)]
                        nil))
      (integer? result) (reply-error! fuse request result)
      true (reply-error! fuse request errno-nosys))))

(defn write-statfs-out
  [statfs-out]
  (domonad
    state-m
    [_ (write-int64 (:blocks statfs-out))
     _ (write-int64 (:bfree statfs-out))
     _ (write-int64 (:bavail statfs-out))
     _ (write-int64 (:files statfs-out))
     _ (write-int64 (:ffree statfs-out))
     _ (write-int32 (:bsize statfs-out))
     _ (write-int32 (:namelen statfs-out))
     _ (write-int32 (:frsize statfs-out))
     _ (pad 28)]
    nil))
 
(defn process-statfs!
  [fuse request arg]
  (let [result (.statfs (:filesystem fuse) request)]
    (cond 
      (map? result) (reply-ok! fuse request (write-statfs-out result))
      (integer? result) (reply-error! fuse request result)
      true (reply-error! fuse request errno-nosys))))

(def parse-init-in
  (domonad
    parser-m
    [major parse-uint32
     minor parse-uint32
     max-readahead parse-uint32
     flags parse-opaque32]
    {:major major
     :minor minor
     :max-readahead max-readahead
     :flags flags}))

(defn write-init-out
  [init-out]
  (domonad state-m
    [_ (write-int32 (:major init-out))
     _ (write-int32 (:minor init-out))
     _ (write-int32 (:max-readahead init-out))
     _ (write-int32 (:flags init-out))
     _ (write-int16 (:max-background init-out))
     _ (write-int16 (:congestion-threshold init-out))
     _ (write-int32 (:max-write init-out))]
    nil))

(defn process-init!
  [fuse request arg]
  (domonad
    maybe-m
    [:let [init (first (parse-init-in arg))]
     _ (if (nil? init)
         (reply-error! fuse request errno-inval)
         :nop)
     :let [connection (:connection fuse)]
     _ (do
         (reset! (:proto-major connection) (:major init))
         (reset! (:proto-minor connection) (:minor init))
         :nop)
     _ (if (> fuse-version-major (:major init))
         ;; kernel is too old, give up
         (reply-error! fuse request errno-proto)
         :nop)
     _ (if (< fuse-version-major (:major init))
         ;; kernel is too new, tell it we want to talk at an earlier version
         (reply-ok! fuse request
           (write-init-out {:major fuse-version-major
                            :minor fuse-version-minor
                            :max-readahead 0
                            :flags 0
                            :max-background 0
                            :congestion-threshold 0
                            :max-write 0}))
         :nop)
     _ (do
         (.init (:filesystem fuse) request)
         (reply-ok! fuse request
           (write-init-out {:major fuse-version-major
                            :minor fuse-version-minor
                            :max-readahead (:max-readahead init)
                            :flags 0
                            :max-background 0
                            :congestion-threshold 0
                            :max-write 0x21000}))
         :nop)
     ] nil))

(def parse-open-in
  (domonad parser-m
    [flags parse-opaque32
     _ skip-32]
    flags))

(defn write-open-out
  [open-out]
  (domonad
    state-m
    [_ (write-int64 (:handle open-out))
     _ (write-int32 (:flags open-out))
     _ (pad 4)]
    nil))

(defn process-opendir!
  [fuse request arg]
  (let [flags (first (parse-open-in arg))]
    (if (nil? flags)
      (reply-error! fuse request errno-inval)
      (let [result (.opendir (:filesystem fuse) request flags)]
        (cond 
          (map? result) (reply-ok! fuse request (write-open-out result))
          (integer? result) (reply-error! fuse request result)
          true (reply-error! fuse request errno-nosys))))))

(def parse-read-in
  (domonad
    parser-m
    [handle parse-opaque64
     offset parse-uint64
     size parse-uint32
     read-flags parse-opaque32
     lock-owner parse-opaque64
     flags parse-opaque32
     _ skip-32]
    {:handle handle
     :offset offset
     :size size
     :read-flags read-flags
     :lock-owner lock-owner
     :flags flags}))

(defn process-readdir!
  [fuse request arg]
  (let [read-in (first (parse-read-in arg))]
    (if (nil? read-in)
      (reply-error! fuse request errno-inval)
      (let [result (.readdir (:filesystem fuse) request read-in)]
        (cond
          (integer? result) (reply-error! fuse request result)
          true (reply-ok! fuse request (write-bytes result)))))))

(def ops
  {op-getattr process-getattr!
   op-statfs process-statfs!
   op-init process-init!
   op-opendir process-opendir!
   op-readdir process-readdir!})

(defn process-buf!
  [fuse buf]
  (domonad maybe-m
    [[request arg] (parse-in-header buf)
     ;; TODO: do something with in-header.len?
     :let [opcode (:opcode request)
           op (ops opcode)]
     _ (if (nil? op)
         (do
           (.write *out* (str "No op for " opcode "\n"))
           (reply-error! fuse request errno-nosys)
           nil)
         :nop)
     _ (op fuse request arg)] nil))

