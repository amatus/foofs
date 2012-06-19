(ns foofs.crypto
  (:use foofs.fuse.bytebuffer)
  (:import java.security.MessageDigest
           javax.crypto.Cipher
           (javax.crypto.spec IvParameterSpec SecretKeySpec)))

(defn sha-512
  [message]
  (.digest (MessageDigest/getInstance "SHA-512") (to-byte-array message)))

(defn make-aes-key
  [key-bytes]
  (SecretKeySpec. (to-byte-array key-bytes) "AES"))

(defn make-aes-iv
  [iv-bytes]
  (IvParameterSpec. (to-byte-array iv-bytes)))

(defn aes-encrypt
  [aes-key iv plaintext-bytes]
  (.doFinal (doto (Cipher/getInstance "AES/CFB/NoPadding")
              (.init Cipher/ENCRYPT_MODE aes-key iv))
            (to-byte-array plaintext-bytes)))

(defn aes-decrypt
  [aes-key iv ciphertext-bytes]
  (.doFinal (doto (Cipher/getInstance "AES/CFB/NoPadding")
              (.init Cipher/DECRYPT_MODE aes-key iv))
            (to-byte-array ciphertext-bytes)))
