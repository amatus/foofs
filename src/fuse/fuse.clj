;;(ns foofs.fuse
;;  (:import (com.sun.jna Function)))
(import 'com.sun.jna.Function)

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

(def fd (open "/dev/fuse" 0100002 0))

(mount "foofs" "/mnt" "fuse.foofs" 7
  (str "fd=" fd ",rootmode=40000,user_id=0,group_id=0"))
