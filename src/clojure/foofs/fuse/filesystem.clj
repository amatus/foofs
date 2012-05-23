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

(ns foofs.fuse.filesystem)

(defprotocol Filesystem
  "A FUSE filesystem."
  (lookup [this request continuation!] "Lookup inode.")
  (forget [this request] "I forget.")
  (getattr [this request continuation!] "Get file attributes.")
  (setattr [this request continuation!] "Set file attributes.")
  (mknod [this request continuation!] "Create node.")
  (mkdir [this request continuation!] "Create directory.")
  (unlink [this request continuation!] "Unlink a file.")
  (rmdir [this request continuation!] "Remove an empty directory.")
  (rename [this request continuation!] "Rename.")
  (link [this request continuation!] "Create hardlink.")
  (open [this request continuation!] "Open file.")
  (readfile [this request continuation!] "Read file.")
  (writefile [this request continuation!] "Write file.")
  (statfs [this request continuation!] "Get file system statistics.")
  (release [this request continuation!] "Release file.")
  (init [this request] "Initialize filesystem.")
  (opendir [this request continuation!] "Open directory.")
  (readdir [this request continuation!] "Read directory.")
  (releasedir [this request continuation!] "Release directory.")
  (create [this request continuation!] "Create file.")
  (destroy [this request] "Clean up filesystem.")
  ;; and so on
  )
