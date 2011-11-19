(ns foofs.fuse.protocol
  (:use (foofs.fuse bytebuffer parser)
        clojure.contrib.monads))

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
     _ (write-int16 (:max-background init-out))
     _ (write-int16 (:congestion-threshold init-out))
     _ (write-int32 (:max-write init-out))]
    nil))

(def ops {})

(defn process-buf
  [buf]
  (domonad maybe-m
    [in (first (parse-in-header buf))
     :let [opcode (:opcode in)
           op (ops opcode)]
     _ (if (nil? op)
         (do (reply-error ENOSYS) nil)
         :nop)
     _ (if (== op-interrupt opcode)
         :nop
         (do (check-interrupt in) :nop))
     _ (op in)]))

