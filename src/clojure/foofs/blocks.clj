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
