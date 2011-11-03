;;(ns foofs.fuse
;;  (:import (com.sun.jna Function)))
(import '(com.sun.jna Function Pointer StringArray)
        'com.sun.jna.ptr.IntByReference)

(defmacro def-jna
  [f lib fname params]
  `(let [func# (Function/getFunction ~lib ~fname)]
     (defn ~f
       ~params
       (.invokeInt func# (to-array ~params)))))

(defmacro def-jna-errno
  [f lib fname params]
  `(let [func# (Function/getFunction ~lib ~fname Function/THROW_LAST_ERROR)]
     (defn ~f
       ~params
       (.invokeInt func# (to-array ~params)))))

(def-jna-errno open "c" "open" [^String path ^Integer flags ^Integer mode])

(def-jna-errno close "c" "close" [^Integer fd])

(def-jna getuid "c" "getuid" [])

(def-jna getgid "c" "getgid" [])

(def-jna-errno mount "c" "mount" [^String source
                                  ^String target
                                  ^String filesystemtype
                                  ^Integer mountflags
                                  ^String data])

(def-jna posix_spawn "c" "posix_spawn" [^IntByReference pid
                                        ^String path
                                        ^Pointer file_actions
                                        ^Pointer attrp
                                        ^StringArray argv
                                        ^StringArray envp])

(def-jna-errno waitpid "c" "waitpid" [^Integer pid
                                      ^IntByReference stat_loc
                                      ^Integer options])

(def fd (open "/dev/fuse" 0100002 0))

;;(mount "foofs" "/mnt" "fuse.foofs" 7
;;  (str "fd=" fd ",rootmode=40000,user_id=0,group_id=0"))

(def pid (IntByReference.))

(posix_spawn
  pid
  "/usr/sbin/mount_fusefs"
  nil
  nil
  (into-array ["mount_fusefs" (str fd) "/mnt"])
  (into-array ["MOUNT_FUSEFS_SAFE=1" "MOUNT_FUSEFS_CALL_BY_LIB=1"]))

(def stat_loc (IntByReference.))

(waitpid (.getValue pid) stat_loc 0)

(close fd)
