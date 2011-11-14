(ns foofs.fuse.bytebuffer
  (:use foofs.fuse.parser))

(defn parse-int32
  [buffer]
  (when (<= 4 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(.getInt buffer2) buffer2])))

(def parse-opaque32 parse-int32)

(defn parse-uint32
  [buffer]
  (when (<= 4 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(bit-and 0xffffffff (.getInt buffer2)) buffer2])))

(defn parse-int64
  [buffer]
  (when (<= 8 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(.getLong buffer2) buffer2])))

(def parse-opaque64 parse-int64)

(defn parse-uint64
  [buffer]
  (when (<= 8 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      [(.and (biginteger 0xffffffffffffffff) (biginteger (.getLong buffer2)))
       buffer2])))

