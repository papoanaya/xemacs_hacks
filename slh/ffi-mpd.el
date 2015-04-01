;; ffi-mpd.el --- elisp binding into libmpd (Music Playing Daemon)   -*- Emacs-Lisp -*-

;; Copyright (C) 2008 Steve Youngs

;; Author:     Steve Youngs <steve@sxemacs.org>
;; Maintainer: Steve Youngs <steve@sxemacs.org>
;; Created:    <2008-06-05>
;; Time-stamp: <Friday Jun  6, 2008 01:37:21 steve>
;; Homepage:   
;; Keywords:   FFI, music

;; This file is part of nothing yet.

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
;;   Interact directly with mpd (Music Playing Daemon) via libmpd.so
;;   with FFI.

;;; Todo:
;;
;;     

;;; Code:
(require 'ffi)
(ffi-load "libmpd")

;; Data structures and types
(define-ffi-struct mpd_Song
  '((:file '(pointer char))
    (:artist '(pointer char))
    (:title '(pointer char))
    (:album '(pointer char))
    (:track '(pointer char))
    (:name '(pointer char))
    (:date '(pointer char))
    (:genre '(pointer char))
    (:composer '(pointer char))
    (:performer '(pointer char))
    (:disc '(pointer char))
    (:comment '(pointer char))
    (:time 'int)
    (:pos 'int)
    (:id 'int)))

(define-ffi-type MpdObj (struct _MpdObj))

(defvar mpd_new_default
  (ffi-defun '(function MpdObj) "mpd_new_default")
  "FFI object for libmpd's mpd_new_default().")

(defun mpd:new_default ()
  "Open new connection to mpd with default settings."
  (ffi-call-function mpd_new_default))

;; Playlist
(defvar mpd_playlist_get_playlist_id
  (ffi-defun '(function '(long long) MpdObj)
	     "mpd_playlist_get_playlist_id")
  "FFI function object mpd_playlist_get_playlist_id(MpdObj *mi).")

(provide 'ffi-mpd)
;;; ffi-mpd.el ends here
