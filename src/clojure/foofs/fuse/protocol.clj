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
      (c-write (:fd fuse) mem (.limit buf))
      (catch Exception e
        (.printStackTrace e))))
  nil)

(defn reply-error!
  [fuse request error]
  (send-reply! fuse request (- error) write-nothing))

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

(def parse-lookup-in parse-utf8)

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

(defn write-entry-out
  [entry-out]
  (domonad
    state-m
    [_ (write-int64 (:nodeid entry-out))
     _ (write-int64 (:generation entry-out))
     _ (write-int64 (:entry-valid entry-out))
     _ (write-int64 (:attr-valid entry-out))
     _ (write-int32 (:entry-valid-nsec entry-out))
     _ (write-int32 (:attr-valid-nsec entry-out))
     _ (write-fuse-attr (:attr entry-out))]
    nil))

(defn process-lookup!
  [fuse request]
  (.lookup
    (:filesystem fuse)
    request
    (fn [result]
      (cond
        (map? result) (reply-ok!
                        fuse
                        request
                        (write-entry-out result))
        (integer? result) (reply-error! fuse request result)))))

(def parse-forget-in
  (domonad
    parser-m
    [nlookup parse-uint64]
    nlookup))

(defn process-forget!
  [fuse request]
  (.forget (:filesystem fuse) request)
  (reply-ok! fuse request write-nothing))

(defn write-attr-out
  [valid valid-nsec]
  (domonad
    state-m
    [_ (write-int64 valid)
     _ (write-int32 valid-nsec)
     _ (pad 4)]
    nil))

(defn process-getattr!
  [fuse request]
  (.getattr
    (:filesystem fuse)
    request
    (fn [result]
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
        true (reply-error! fuse request errno-nosys)))))

(def parse-mknod-in
  (domonad
    parser-m
    [mode parse-opaque32
     rdev parse-opaque32
     filename parse-utf8]
    {:mode mode
     :rdev rdev
     :filename filename}))

(defn process-mknod!
  [fuse request]
  (.mknod
    (:filesystem fuse)
    request
    (fn [result]
      (cond
        (map? result) (reply-ok!
                        fuse
                        request
                        (write-entry-out result))
        (integer? result) (reply-error! fuse request result)))))

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

(defn process-open!
  [fuse request]
  (.open
    (:filesystem fuse)
    request
    (fn [result]
      (cond 
        (map? result) (reply-ok! fuse request (write-open-out result))
        (integer? result) (reply-error! fuse request result)
        true (reply-error! fuse request errno-nosys)))))

(def parse-read-in
  (domonad
    parser-m
    [handle parse-opaque64
     offset parse-uint64
     size parse-uint32
     read-flags parse-opaque32]
    {:handle handle
     :offset offset
     :size size
     :read-flags read-flags}))

(defn process-read!
  [fuse request]
  (.readfile
    (:filesystem fuse)
    request
    (fn [result]
      (if (integer? result)
        (reply-error! fuse request result)
        (reply-ok! fuse request (write-bytes result))))))

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
  [fuse request]
  (.statfs
    (:filesystem fuse)
    request
    (fn [result]
      (cond 
        (map? result) (reply-ok! fuse request (write-statfs-out result))
        (integer? result) (reply-error! fuse request result)
        true (reply-error! fuse request errno-nosys)))))

(def parse-release-in
  (domonad
    parser-m
    [handle parse-opaque64
     flags parse-opaque32
     release-flags parse-opaque32]
    {:handle handle
     :flags flags
     :release-flags release-flags}))

(defn process-release!
  [fuse request]
  (.release
    (:filesystem fuse)
    request
    (fn [result]
      (if (integer? result)
        (reply-error! fuse request result)
        (reply-error! fuse request 0)))))

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
  [fuse request]
  (domonad
    maybe-m
    [:let [init (:arg request)
           connection (:connection fuse)]
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

(defn process-opendir!
  [fuse request]
  (.opendir
    (:filesystem fuse)
    request
    (fn [result]
      (cond 
        (map? result) (reply-ok! fuse request (write-open-out result))
        (integer? result) (reply-error! fuse request result)
        true (reply-error! fuse request errno-nosys)))))

(def name-offset 24)
(defn dirent-align
  [x]
  (* 8 (quot (+ x 7) 8)))

(defn encode-dirent
  [dirent offset]
  (let [namebytes (.getBytes (:name dirent) "UTF-8")
        namelen (count namebytes)
        entsize (dirent-align (+ name-offset namelen))]
    (take
      entsize
      (concat
        (encode-int64 (:nodeid dirent))
        (encode-int64 (+ offset entsize))
        (encode-int32 namelen)
        (encode-int32 (bit-shift-right (bit-and stat-type-mask (:type dirent))
                                       12))
        namebytes
        (repeat 0)))))

(defn encode-dirents
  [dirents]
  (first (reduce (fn
                   [state dirent]
                   (let [[encoded-dirents offset] state
                         encoded-dirent (encode-dirent dirent offset)]
                     [(concat encoded-dirents encoded-dirent)
                      (+ offset (count encoded-dirent))]))
                 [[] 0]
                 dirents)))

(defn process-readdir!
  [fuse request]
  (.readdir
    (:filesystem fuse)
    request
    (fn [result]
      (if (integer? result)
        (reply-error! fuse request result)
        (reply-ok! fuse request (write-bytes result))))))

(defn process-releasedir!
  [fuse request]
  (.releasedir
    (:filesystem fuse)
    request
    (fn [result]
      (if (integer? result)
        (reply-error! fuse request result)
        (reply-error! fuse request 0)))))

(def parse-create-in
  (domonad
    parser-m
    [flags parse-opaque32
     mode parse-opaque32
     filename parse-utf8]
    {:flags flags
     :mode mode
     :filename filename}))

(defn write-create-out
  [create-out]
  (domonad state-m
    [_ (write-entry-out create-out)
     _ (write-open-out create-out)]
    nil))

(defn process-create!
  [fuse request]
  (.create
    (:filesystem fuse)
    request
    (fn [result]
      (cond 
        (map? result) (reply-ok! fuse request (write-create-out result))
        (integer? result) (reply-error! fuse request result)
        true (reply-error! fuse request errno-nosys)))))

(defn process-destroy!
  [fuse request]
  (.destroy (:filesystem fuse) request)
  (c-close (:fd fuse)))

(def ops
  {op-lookup {:arg-parser parse-lookup-in
              :processor! process-lookup!}
   op-forget {:arg-parser parse-forget-in
              :processor! process-forget!}
   op-getattr {:arg-parser parse-nothing
               :processor! process-getattr!}
   op-mknod {:arg-parser parse-mknod-in
             :processor! process-mknod!}
   op-open {:arg-parser parse-open-in
            :processor! process-open!}
   op-read {:arg-parser parse-read-in
            :processor! process-read!}
   op-statfs {:arg-parser parse-nothing
              :processor! process-statfs!}
   op-release {:arg-parser parse-release-in
               :processor! process-release!}
   op-init {:arg-parser parse-init-in
            :processor! process-init!}
   op-opendir {:arg-parser parse-open-in
               :processor! process-opendir!}
   op-readdir {:arg-parser parse-read-in
               :processor! process-readdir!}
   op-releasedir {:arg-parser parse-release-in
                  :processor! process-releasedir!}
   op-create {:arg-parser parse-create-in
              :processor! process-create!}
   op-destroy {:arg-parser parse-nothing
               :processor! process-destroy!}})

(defn process-buf!
  [fuse buf]
  (domonad maybe-m
    [[request arg-buf] (parse-in-header buf)
     ;; TODO: do something with in-header.len?
     :let [opcode (:opcode request)
           op (ops opcode)]
     _ (if (nil? op)
         (do
           (.println *err* (str "No op for " request))
           (.println *err* (hexdump arg-buf))
           (reply-error! fuse request errno-nosys)
           nil)
         :nop)
     :let [argx ((:arg-parser op) arg-buf)]
     _ (if (nil? argx)
         (do
           (.println *err* (str "Invalid arg " (hexdump arg-buf)
                                " for " opcode))
           (reply-error! fuse request errno-inval))
         :nop)
     _ ((:processor! op) fuse (assoc request :arg (first argx)))] nil))

