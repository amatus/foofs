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

(ns foofs.fuse.testfs
  (:use (foofs.fuse fuse jna protocol)))

(def test-fs
  (let [root-node-attr {:inode 1
                        :size 0
                        :blocks 0
                        :atime 0
                        :mtime 0
                        :ctime 0
                        :atimensec 0
                        :mtimensec 0
                        :ctimensec 0
                        :mode 040000
                        :nlink 1
                        :uid 0
                        :gid 0
                        :rdev 0}]
    (reify Filesystem
      (lookup [this request continuation!]
        (.println *err* (str "lookup: " request))
        (continuation!
          {:nodeid 1
           :generation 0
           :entry-valid 0
           :attr-valid 0
           :entry-valid-nsec 0
           :attr-valid-nsec 0
           :attr root-node-attr}))
      (getattr [this request continuation!]
        (.println *err* (str "getattr: " request))
        (continuation! root-node-attr))
      (statfs [this request continuation!]
        (continuation!
         {:blocks 0
          :bfree 0
          :bavail 0
          :files 0
          :ffree 0
          :bsize 512
          :namelen 255
          :frsize 0}))
      (init [this request]
        (.println *err* "init called."))
      (opendir [this request continuation!]
        (continuation!
          {:handle 1
           :flags 0}))
      (readdir [this request continuation!]
        (let [dirent (encode-dirent
                       {:nodeid 1
                        :type stat-type-directory
                        :name "test"})
              result (take (:size (:arg request))
                           (drop (:offset (:arg request))
                                 dirent))]
          (continuation! result)))
      (releasedir [this request continuation!]
        (continuation! nil))
      (destroy [this request]
        (.println *err* "destroy called.")))))
