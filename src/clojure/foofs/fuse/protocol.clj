(ns foofs.fuse.protocol
  (:use (foofs.fuse bytebuffer jna parser)
        clojure.contrib.monads)
  (:import com.sun.jna.Memory))

(def fuse-version-major 7)
(def fuse-version-minor 17)

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

(defn send-reply
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

(defn reply-error
  [fuse request error]
  (send-reply fuse request error (with-monad state-m m-zero)))

(defn reply-ok
  [fuse request reply]
  (send-reply fuse request 0 reply))

(defrecord in-header
  [^long len
   ^int opcode
   ^long unique
   ^long nodeid
   ^int uid
   ^int gid
   ^int pid])

(def parse-in-header
  (domonad parser-m
    [len parse-uint32
     opcode parse-opaque32
     unique parse-opaque64
     nodeid parse-opaque64
     uid parse-opaque32
     gid parse-opaque32
     pid parse-opaque32
     _ (skip 4)]
    (in-header. len opcode unique nodeid uid gid pid)))

(defrecord init-in
  [^long major
   ^long minor
   ^long max-readahead
   ^int flags])

(def parse-init-in
  (domonad parser-m
    [major parse-uint32
     minor parse-uint32
     max-readahead parse-uint32
     flags parse-opaque32]
    (init-in. major minor max-readahead flags)))

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
         (reply-error fuse request errno-inval)
         :nop)
     :let [connection (:connection fuse)]
     _ (do
         (reset! (:proto-major connection) (:major init))
         (reset! (:proto-minor connection) (:minor init))
         :nop)
     _ (if (> fuse-version-major (:major init))
         ;; kernel is too old, give up
         (reply-error fuse request errno-proto)
         :nop)
     _ (if (< fuse-version-major (:major init))
         ;; kernel is too new, tell it we want to talk at an earlier version
         (reply-ok fuse request
           (write-init-out {:major fuse-version-major
                            :minor fuse-version-minor
                            :max-readahead 0
                            :flags 0
                            :max-background 0
                            :congestion-threshold 0
                            :max-write 0}))
         :nop)
     _ (do
         (.init (:filesystem fuse))
         (reply-ok fuse request
           (write-init-out {:major fuse-version-major
                            :minor fuse-version-minor
                            :max-readahead (:max-readahead init)
                            :flags 0
                            :max-background 0
                            :congestion-threshold 0
                            :max-write 0x21000}))
         :nop)
     ] nil))

(def ops
  {op-init process-init!})

(defn process-buf
  [fuse buf]
  (domonad maybe-m
    [[request arg] (parse-in-header buf)
     ;; TODO: do something with in-header.len?
     :let [opcode (:opcode request)
           op (ops opcode)]
     _ (if (nil? op)
         (do (reply-error fuse request errno-nosys) nil)
         :nop)
     _ (op fuse request arg)] nil))

