(ns foofs.fuse.bytebuffer
  (:use clojure.contrib.monads
        foofs.fuse.parser)
  (:import java.nio.ByteBuffer))

(defn parse-int32
  [buffer]
  (when (<= 4 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      (.order buffer2 (.order buffer)) ;; this is a damn shame right here
      [(.getInt buffer2) buffer2])))

(def parse-opaque32 parse-int32)

(defn parse-uint32
  [buffer]
  (when (<= 4 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      (.order buffer2 (.order buffer))
      [(bit-and 0xffffffff (.getInt buffer2)) buffer2])))

(defn parse-int64
  [buffer]
  (when (<= 8 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      (.order buffer2 (.order buffer))
      [(.getLong buffer2) buffer2])))

(def parse-opaque64 parse-int64)

(defn parse-uint64
  [buffer]
  (when (<= 8 (.remaining buffer))
    (let [buffer2 (.duplicate buffer)]
      (.order buffer2 (.order buffer))
      [(.and (biginteger 0xffffffffffffffff) (biginteger (.getLong buffer2)))
       buffer2])))

(defn skip
  [x]
  (fn parse-skip
    [buffer]
    (let [position (.position buffer)
          buffer2 (.duplicate buffer)]
      (.order buffer2 (.order buffer))
      [nil (.position buffer2 (+ x position))])))

(def skip-32
  (skip 4))

(def skip-64
  (skip 8))

(def parse-nothing (with-monad parser-m (m-result nil)))

(defn write-int16
  [x]
  (fn write-int16!
    [buffer]
    [nil (.putShort buffer x)]))

(defn write-int32
  [x]
  (fn write-int32!
    [buffer]
    [nil (.putInt buffer x)]))

(defn write-int64
  [x]
  (fn write-int64!
    [buffer]
    [nil (.putLong buffer x)]))

(def bytes-class (class (byte-array 0)))
(defn write-bytes
  [x]
  (cond
    (instance? bytes-class x) (fn write-byte-array!
                                [buffer]
                                [nil (.put buffer x)])
    (instance? ByteBuffer x) (fn write-byte-buffer!
                               [buffer]
                               [nil (.put buffer (.array x))])
    (every? number? x)
    (fn write-byte-seq!
      [buffer]
      (let [bytes-array (into-array Byte/TYPE (map #(.byteValue %) x))]
        [nil (.put buffer bytes-array)]))))

(defn pad
  [x]
  (fn pad!
    [buffer]
    (let [position (.position buffer)]
      [nil (.position buffer (+ x position))])))

(def write-nothing (with-monad state-m (m-result nil)))

(defn buffer-seq!
  "Returns a lazy seq that consumes a subclass of java.nio.Buffer."
  [buffer]
  (lazy-seq (when (.hasRemaining buffer)
              (cons (.get buffer) (buffer-seq! buffer)))))

(defn hexdigit
  [x]
  (.charAt "0123456789ABCDEF" x))

(defn hexdump
  [buffer]
  (apply str
         (interpose
           " "
           (map #(str (hexdigit (quot % 256)) (hexdigit (quot % 256)))
                (buffer-seq! (.duplicate buffer))))))
