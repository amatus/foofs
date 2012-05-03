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

(ns foofs.fuse.jna
  (:import (com.sun.jna Function Memory NativeLong Platform Pointer StringArray)
           com.sun.jna.ptr.IntByReference
           java.nio.ByteOrder))

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

(def-jna c-open ["c" "open" Function/THROW_LAST_ERROR]
  [path flags mode]
  (assert-args open
    (string? path) "path is a String"
    (integer? flags) "flags is an integer"
    (integer? mode) "mode is an integer")
  [path (int32_t flags) (int32_t mode)])

(def-jna c-close ["c" "close" Function/THROW_LAST_ERROR]
  [fd]
  (assert-args close
    (integer? fd) "fd is an integer")
  [(int32_t fd)])

(def-jna c-read ["c" "read" Function/THROW_LAST_ERROR]
  [fd buf nbytes]
  (assert-args c-read 
    (integer? fd) "fd is an integer"
    (pointer? buf) "buf is a pointer"
    (integer? nbytes) "nbytes is an integer")
  [(int32_t fd) buf (size_t nbytes)])

(def-jna c-write ["c" "write" Function/THROW_LAST_ERROR]
  [fd buf nbytes]
  (assert-args c-write
    (integer? fd) "fd is an integer"
    (pointer? buf) "buf is a pointer"
    (integer? nbytes) "nbytes is an integer")
  [(int32_t fd) buf (size_t nbytes)])

(def-jna getuid ["c" "getuid"]
  [])

(def-jna getgid ["c" "getgid"]
  [])

(def-jna socketpair ["c" "socketpair" Function/THROW_LAST_ERROR]
  [domain type protocol sv]
  (assert-args socketpair
    (integer? domain) "domain is an integer"
    (integer? type) "type is an integer"
    (integer? protocol) "protocol is an integer"
    (pointer? sv) "sv is a pointer")
  [(int32_t domain) (int32_t type) (int32_t protocol) sv])

;; I thought this should THROW_LAST_ERROR but it keeps throwing EINVAL
;; when it's actually working.
(def-jna recvmsg ["c" "recvmsg"]
  [sockfd msg flags]
  (assert-args recvmsg
    (integer? sockfd) "sockfd is an integer"
    (pointer? msg) "msg is a pointer"
    (integer? flags) "flags is an integer")
  [(int32_t sockfd) msg (int32_t flags)])

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

(def-jna posix_spawnp ["c" "posix_spawnp"]
  [pid file file_actions attrp argv envp]
  (assert-args posix_spawn
    (instance? IntByReference pid) "pid is an IntByReference"
    (string? file) "file is a string"
    (pointer? file_actions) "file_actions is a Pointer"
    (pointer? attrp) "attrp is a Pointer"
    (every? string? argv) "argv is a sequence of Strings"
    (every? string? envp) "envp is a sequence of Strings")
  [pid file file_actions attrp (into-array argv) (into-array envp)])

(def-jna waitpid ["c" "waitpid" Function/THROW_LAST_ERROR]
  [pid stat_loc options]
  (assert-args waitpid
    (integer? pid) "pid is an integer"
    (instance? IntByReference stat_loc) "stat_loc is an IntByReference"
    (integer? options) "options is an integer")
  [pid stat_loc options])

(when (Platform/isLinux)
  (do
    (def errno-noent 2)
    (def errno-nomem 12)
    (def errno-exist 17)
    (def errno-notdir 20)
    (def errno-isdir 21)
    (def errno-inval 22)
    (def errno-nosys 38)
    (def errno-notempty 39)
    (def errno-proto 71)
    (def pf-unix 1)
    (def sock-stream 1)
    ;; XXX these are all really bad
    (def sizeof-msghdr 28)
    (def offsetof-msg_iov 8)
    (def offsetof-msg_iovlen 12)
    (def offsetof-msg_control 16)
    (def offsetof-msg_controllen 20)
    (def sizeof-iovec 8)
    (def offsetof-iov_base 0)
    (def offsetof-iov_len 4)
    (def scm-rights 1)))

(when (Platform/isFreeBSD)
  (do
    (def errno-noent 2)
    (def errno-nomem 12)
    (def errno-exist 17)
    (def errno-notdir 20)
    (def errno-inval 22)
    (def errno-nosys 78)
    (def errno-proto 92)))

(def byte-order (.order (.getByteBuffer (Memory. 1) 0 1)))

(if (= ByteOrder/BIG_ENDIAN byte-order)
  (do
    (defn encode-int16
      [x]
      (list (.byteValue (bit-shift-right x 8)) (.byteValue (bit-and x 0xFF))))
    (defn encode-int32
      [x]
      (concat (encode-int16 (bit-shift-right x 16))
              (encode-int16 (bit-and x 0xFFFF))))
    (defn encode-int64
      [x]
      (concat (encode-int32 (bit-and (bit-shift-right x 32) 0xFFFFFFFF))
              (encode-int32 (bit-and x 0xFFFFFFFF)))))
  (do
    (defn encode-int16
      [x]
      (list (.byteValue (bit-and x 0xFF)) (.byteValue (bit-shift-right x 8))))
    (defn encode-int32
      [x]
      (concat (encode-int16 (bit-and x 0xFFFF))
              (encode-int16 (bit-shift-right x 16))))
    (defn encode-int64
      [x]
      (concat (encode-int32 (bit-and x 0xFFFFFFFF))
              (encode-int32 (bit-and (bit-shift-right x 32) 0xFFFFFFFF))))))

(def stat-type-fifo      0010000)
(def stat-type-character 0020000)
(def stat-type-directory 0040000)
(def stat-type-block     0060000)
(def stat-type-regular   0100000)
(def stat-type-link      0120000)
(def stat-type-socket    0140000)
(def stat-type-whiteout  0160000)
(def stat-type-mask      0170000)

;; TODO: Figure out how to subclass com.sun.jna.Structure in Clojure or wimp
;; out and write it in Java.
(defn receive-fd
  [sockfd]
  (let [msg (Memory. sizeof-msghdr)
        iov (Memory. sizeof-iovec)
        buf (Memory. 1)
        ccmsg (Memory. 16)]
    (.setPointer iov offsetof-iov_base buf)
    (.setInt iov offsetof-iov_len 1)
    (.setPointer msg offsetof-msg_iov iov)
    (.setInt msg offsetof-msg_iovlen (.size iov))
    (.setPointer msg offsetof-msg_control ccmsg)
    (.setInt msg offsetof-msg_controllen (.size ccmsg))
    (recvmsg sockfd msg 0) ;; WTF: even when this returns -1 it seems to work
    (when (== scm-rights (.getInt ccmsg 8))
      (.getInt ccmsg 12))))
