(ns foofs.blocks
  (:use foofs.crypto))

(def aes-key-size 32)
(def aes-iv-size 16)

(defn encode-block
  [f-block salt]
  (let [e-key (sha-512 f-block)
        aes-key (make-aes-key (take aes-key-size e-key))
        iv (make-aes-iv (take aes-iv-size (concat salt (repeat 0))))
        e-block (aes-encrypt aes-key iv f-block)
        e-hash (sha-512 e-block)]
    [e-key e-hash e-block]))

(defn decode-block
  [e-block e-key salt]
  (let [aes-key (make-aes-key (take aes-key-size e-key))
        iv (make-aes-iv (take aes-iv-size (concat salt (repeat 0))))
        f-block (aes-decrypt aes-key iv e-block)]
    f-block))
