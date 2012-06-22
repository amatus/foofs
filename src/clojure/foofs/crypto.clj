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
