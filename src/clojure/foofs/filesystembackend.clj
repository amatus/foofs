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

(ns foofs.filesystembackend)

(defprotocol FilesystemBackend
  "The interface provided by a Foofs backend."
  (lookup [this inode child continuation!]
          "Lookup the inode of a directory entry.")
  (getattr [this inode continuation!]
           "Get the attributes of an inode.")
  (reference [this inode continuation!]
             "Increment link count of an inode.")
  (dereference [this inode continuation!]
               "Decrement link count of an inode.")
  (clonedir [this inode continuation!]
            "Return a lazy sequence of directory entries.")
  (readfile [this inode offset size continuation!]
            "Return a ByteBuffer or sequence of bytes.")
  (mknod [this inode filename mode flags continuation!]
         "Create an inode and return its attributes.")
  ;; and so on
  )
