;; linux-kernel.el --- Linux kernel related bits 'n' pieces   -*- Emacs-Lisp -*-

;; Copyright (C) 2003 Steve Youngs

;; RCS: $Id: linux-kernel.el,v 1.1 2003-12-15 08:39:02+10 steve Exp $
;; Author:        Steve Youngs <sryoungs@bigpond.net.au>
;; Maintainer:    Steve Youngs <sryoungs@bigpond.net.au>
;; Created:       <2003-12-15>
;; Last-Modified: <2003-12-15 08:38:58 (steve)>
;; Homepage:      None
;; Keywords:      kernel linux

;; This file is part of linux-kernel.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; 
;;   Here is a collection of things I find useful in the land of Linux
;;   kernels.
;;
;;  Currently implemented features:
;;
;;    - Check the latest kernel versions `linux-kernel-check-latest'

;;; Todo:
;;
;;     o View kernel ChangeLog-<version> files.
;;
;;     o Download official kernel patches (possibly entire kernels
;;       too).
;;
;;     o Apply/revert patches to local workspace.
;;
;;     o Create a TAGS table file that will actually work in XEmacs
;;       (the kernel's `make tags' doesn't work for me and my XEmacs
;;       :-( ).
;;
;;     o Ensure that cc-mode is set up the way Linus likes when hacking
;;       the kernel.
;;
;;     o Configure the kernel from within XEmacs 
;;       (`make ([xg]|menu)?config').

;;; ChangeLog:
;;
;; $Log: linux-kernel.el,v $
;; Revision 1.1  2003-12-15 08:39:02+10  steve
;; Initial revision
;;

;;; Code:
(eval-and-compile
  (require 'working)
  (autoload 'with-electric-help "ehelp"))

;;;###autoload
(defun linux-kernel-commentary ()
  "*Display the commentary section of linux-kernel.el."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert (lm-commentary (locate-library "linux-kernel.el")))
	 (goto-char (point-min))
	 (while (re-search-forward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Linux-Kernel Commentary*"))

;;;###autoload
(defun linux-kernel-copyright ()
  "*Display the copyright notice for Linux-Kernel."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert-file-contents (locate-library "linux-kernel.el"))
	 (goto-char (point-min))
	 (re-search-forward ";;; Commentary" nil t)
	 (beginning-of-line)
	 (narrow-to-region (point-min) (point))
	 (while (re-search-backward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Linux-Kernel Copyright Notice*"))

;;;###autoload
(defun linux-kernel-check-latest ()
  "Display a list of the latest kernel versions."
  (interactive)
  (let* ((host "www.kernel.org")
	 (dir "/kdist/")
	 (file "finger_banner")
	 (path (concat dir file))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (user-agent (concat "XEmacs " emacs-program-version))
	 (http
	  (open-network-stream
	   "latest-kernel-proc"
	   " *kernel-proc-buf*"
	   host
	   80))
	 (pbuf (process-buffer http))
	 (obuf (get-buffer-create "*Latest Kernels*")))
    (process-send-string
     http
     (concat "GET " path " HTTP/1.1\r\n"
	     "MIME-Version: 1.0\r\n"
	     "Connection: close\r\n"
	     "Extension: Security/Digest Security/SSL\r\n"
	     "Host: " host "\r\n"
	     "Accept: */*\r\n"
	     "User-Agent: " user-agent "\r\n\r\n"))
    (working-status-forms "Checking Kernel Version: " "Done!"
      (while (eq (process-status http) 'open)
	(working-dynamic-status nil)
	(sleep-for 0.05))
      (working-dynamic-status t))
    (with-electric-help
     '(lambda ()
	(insert
	 (with-current-buffer pbuf
	   (goto-char (point-min))
	   (while (re-search-forward "\r" nil t) nil)
	   (kill-region (point-min) (point))
	   (insert "The Latest Linux Kernels\n========================\n\n")
	   (goto-char (point-min))
	   (center-line 2)
	   (re-search-forward "^Process.*$" nil t)
	   (replace-match "")
	   (buffer-string (current-buffer)))))
     obuf)
    (kill-buffer pbuf)))
  
(provide 'linux-kernel)
;;; linux-kernel.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
