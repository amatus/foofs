;;(ns foofs.fuse
;;  (:import (com.sun.jna Function)))
(import '(com.sun.jna Function Memory))

(let [_open (Function/getFunction "c" "open" Function/THROW_LAST_ERROR)]
  (defn jna-open
    [path flags mode]
    (.invokeInt _open (to-array [path flags mode]))))

(let [_close (Function/getFunction "c" "close" Function/THROW_LAST_ERROR)]
  (defn jna-close
    [fd]
    (.invokeInt _close (to-array [fd]))))

(let [_getuid (Function/getFunction "c" "getuid")]
  (defn jna-getuid
    []
    (.invokeInt _getuid (to-array []))))

(let [_getgid (Function/getFunction "c" "getgid")]
  (defn jna-getgid
    []
    (.invokeInt _getgid (to-array []))))

(let [_mount (Function/getFunction "c" "mount" Function/THROW_LAST_ERROR)]
  (defn jna-mount
    [source target filesystemtype mountflags data]
    (.invokeInt _mount
                (to-array [source target filesystemtype mountflags data]))))

(def fd (jna-open "/dev/fuse" 0100002 0))

(jna-mount "foofs" "/mnt" "fuse.foofs" (Integer. 7)
  (str "fd=" fd ",rootmode=40000,user_id=0,group_id=0"))
