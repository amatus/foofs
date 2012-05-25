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
  (lookup [this nodeid child continuation!]
          "Lookup the nodeid of a directory entry.")
  (getattr [this nodeid continuation!]
           "Get an inode.")
  (reference [this nodeid continuation!]
             "Increment link count of an inode.")
  (dereference [this nodeid continuation!]
               "Decrement link count of an inode.")
  (clonedir [this nodeid continuation!]
            "Return a lazy sequence of directory entries.")
  (readfile [this nodeid offset size continuation!]
            "Return a ByteBuffer or sequence of bytes.")
  (writefile [this nodeid offset size data continuation!]
             "Writes a ByteBuffer to a file.")
  (mknod [this nodeid filename mode continuation!]
         "Create an inode and return it.")
  (link [this nodeid filename target-nodeid continuation!]
        "Create a hardlink and return the target inode.")
  (unlink [this nodeid filename continuation!]
          "Unlink a file.")
  (rmdir [this nodeid filename continuation!]
         "Remove an empty directory.")
  (chmod [this nodeid mode continuation!])
  (setuid [this nodeid uid continuation!])
  (setgid [this nodeid gid continuation!])
  (truncate [this nodeid size continuation!])
  (setatime [this nodeid seconds nseconds continuation!])
  (setmtime [this nodeid seconds nseconds continuation!])
  (rename [this nodeid target-nodeid filename target-filename continuation!]))
