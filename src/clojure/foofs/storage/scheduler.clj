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

(ns foofs.storage.scheduler
  (:use [foofs blocks crypto])
  (:import java.util.concurrent.Executor))

(defprotocol Scheduler
  (fetch-block [this salt block block-size n k continuation!]))

(defrecord BasicScheduler
  [^Executor executor
   read-block]
  Scheduler
  (fetch-block [_ salt block block-size n k continuation!]
    (.execute
      executor
      (fn []
        (let [[e-hash e-key] block
              e-block (read-block e-hash block-size n k)
              test-hash (sha-512 e-block)]
          (if (= e-hash test-hash)
            (let [f-block (decode-block e-block e-key salt)
                  test-hash (sha-512 f-block)]
              (if (= e-key test-hash)
                (continuation! f-block)
                (continuation! nil)))
            (continuation! nil)))))))