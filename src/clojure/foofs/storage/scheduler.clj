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
  (:import java.nio.ByteBuffer java.util.concurrent.Executor))

(defprotocol Scheduler
  (fetch-block [this salt block block-size n k continuation!])
  (store-block [this salt block-bytes n k continuation!]))

(defrecord BasicScheduler
  [^clojure.lang.Agent state-agent
   ^Executor executor
   read-block
   write-block]
  Scheduler
  (fetch-block [_ salt block block-size n k continuation!]
    (.execute
      executor
      (fn []
        (send
          state-agent
          (fn [state]
            ;; check to see if this block is already being fetched
            (if-let [continuations (get-in state [:fetching block])]
              ;; add ourselves to the continuation list
              (assoc-in state [:fetching block]
                        (conj continuations continuation!))
              ;; else start fetching it using the executor
              (do
                (.execute
                  executor
                  (fn []
                    (let [result
                          (let [[e-hash e-key] block
                                e-block (read-block e-hash block-size n k)
                                test-hash (sha-512 e-block)]
                            (when (= (seq e-hash) (seq test-hash))
                              (let [f-block (decode-block e-block e-key salt)
                                    test-hash (sha-512 f-block)]
                                (when (= (seq e-key) (seq test-hash))
                                  (ByteBuffer/wrap f-block)))))]
                      ;; now that we've got it, call all of the continuations
                      (send
                        state-agent
                        (fn [state]
                          (doseq [continuation! (get-in state
                                                        [:fetching block])]
                            (continuation! result))
                          ;; and remove the fetching state
                          (assoc state :fetching
                                 (dissoc (:fetching state) block)))))))
                ;; and add ourselves to the continuation list
                (assoc-in state [:fetching block] [continuation!]))))))))
  (store-block [_ salt f-block n k continuation!]
    (.execute
      executor
      (fn []
        (let [[e-key e-hash e-block] (encode-block f-block salt)]
          (if (write-block e-hash e-block n k)
            (continuation! [e-hash e-key])
            (continuation! nil)))))))
