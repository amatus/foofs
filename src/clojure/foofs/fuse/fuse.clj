;;(ns foofs.fuse
;;  (:import (com.sun.jna Function)))
(import '(com.sun.jna Function Memory NativeLong Pointer StringArray)
        'com.sun.jna.ptr.IntByReference)

(defmacro assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defmacro def-jna
  [f jna-args args & body]
  `(let [func# (Function/getFunction ~@jna-args)]
     (defn ~f
       ~args
       (.invokeInt func# (to-array (do ~@body))))))

(defn pointer?
  [ptr]
  (or (nil? ptr) (instance? Pointer ptr)))

(defn int32_t
  "Converts x to int32_t. For some reason clojure.core/int doesn't."
  [x]
  (Integer. (int x)))

(defn size_t
  "Converts x to native size_t."
  [x]
  ;; on all the machines I've checked size_t is equivalent to unsigned long
  (NativeLong. (long x)))

;; on all the machines I've checked pid_t is equivalent to int32_t
(def pid_t int32_t)

(def-jna open ["c" "open" Function/THROW_LAST_ERROR]
  [path flags mode]
  (assert-args open
    (string? path) "path is a String"
    (integer? flags) "flags is an integer"
    (integer? mode) "mode is an integer")
  [path (int32_t flags) (int32_t mode)])

(def-jna close ["c" "close" Function/THROW_LAST_ERROR]
  [fd]
  (assert-args close
    (integer? fd) "fd is an integer")
  [(int32_t fd)])

(def-jna c-read ["c" "read" Function/THROW_LAST_ERROR]
  [fd buf nbyte]
  (assert-args c-read 
    (integer? fd) "fd is an integer"
    (pointer? buf) "buf is a pointer"
    (integer? nbyte) "nbyte is an integer")
  [(int32_t fd) buf (size_t nbyte)])

(def-jna getuid ["c" "getuid"]
  [])

(def-jna getgid ["c" "getgid"]
  [])

;; This is the type signature for Linux, BSD is different.
(def-jna mount ["c" "mount" Function/THROW_LAST_ERROR]
  [source target filesystemtype mountflags data]
  (assert-args mount
    (string? source) "source is a String"
    (string? target) "target is a String"
    (string? filesystemtype) "filesystemtype is a String"
    (integer? mountflags) "mountflags is an integer"
    (string? data) "data is a String")
  [source target filesystemtype (int32_t mountflags) data])

(def-jna posix_spawn ["c" "posix_spawn"]
  [pid path file_actions attrp argv envp]
  (assert-args posix_spawn
    (instance? IntByReference pid) "pid is an IntByReference"
    (string? path) "path is a string"
    (pointer? file_actions) "file_actions is a Pointer"
    (pointer? attrp) "attrp is a Pointer"
    (every? string? argv) "argv is a sequence of Strings"
    (every? string? envp) "envp is a sequence of Strings")
  [pid path file_actions attrp (into-array argv) (into-array envp)])

(def-jna waitpid ["c" "waitpid" Function/THROW_LAST_ERROR]
  [pid stat_loc options]
  (assert-args waitpid
    (integer? pid) "pid is an integer"
    (instance? IntByReference stat_loc) "stat_loc is an IntByReference"
    (integer? options) "options is an integer")
  [pid stat_loc options])

(defn mem-seq
  [mem]
  (map #(.getByte mem %) (range 0 (.size mem))))

(defn read-loop!
  [fd]
  (try
    (let [buf (Memory. 0x21000)
          ret (c-read fd buf (.size buf))]
      ;; There should be a threshold under which we copy the memory instead
      ;; of holding onto a reference via the mem-seq.
      (.write *out* (str "read " (take ret (mem-seq buf)))))
    (recur fd)
    (catch Exception e
      nil)))

(def fd (open "/dev/fuse" 0100002 0))

;;(mount "foofs" "/mnt" "fuse.foofs" 7
;;  (str "fd=" fd ",rootmode=40000,user_id=0,group_id=0"))

(def pid (IntByReference.))

(posix_spawn
  pid
  "/usr/sbin/mount_fusefs"
  nil
  nil
  ["mount_fusefs" (str fd) "/mnt"]
  ["MOUNT_FUSEFS_SAFE=1" "MOUNT_FUSEFS_CALL_BY_LIB=1"])

(def stat_loc (IntByReference.))

(waitpid (.getValue pid) stat_loc 0)

(close fd)
