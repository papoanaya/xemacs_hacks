;; hddtemp.el --- Display hard disc temperatures   -*- Emacs-Lisp -*-

;; Copyright (C) 2008 Steve Youngs

;; Author:     Steve Youngs <steve@sxemacs.org>
;; Maintainer: Steve Youngs <steve@sxemacs.org>
;; Created:    <2008-08-13>
;; Time-stamp: <Wednesday Aug 13, 2008 16:39:01 steve>
;; Homepage:   
;; Keywords:   sensor

;; This file is part of hddtemp.

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
;;    Just a bit of fun hacking to display hard disc temperatures.  This
;;    requires a util called "hddtemp" to be running in daemon mode.  You
;;    can get it from <http://savannah.nongnu.org/projects/hddtemp/>.

;;; Todo:
;;
;;     o Clean up the butt-ugly let form in #'hddtemp

;;; Code:
(require 'cl-loop)
(put 'cl:dotimes 'lisp-indent-function 'defun)

(defvar hddtemp-hash (make-hash-table :test #'equal :size 4)
  "A hash-table to hold the temp values.")

(defun hddtemp-filt (proc string)
  "Process the output from `hddtemp-proc'."
  (let* ((data (remove "" (split-string-by-char string ?|)))
	 (iterations (/ (length data) 4)))
    (cl:dotimes (i (declare-boundp iterations))
      (setq i (number-to-string i))
      (puthash (concat "dev-" i) (car data) hddtemp-hash)
      (puthash (concat "mod-" i) (cadr data) hddtemp-hash)
      (puthash (concat "tmp-" i) (caddr data) hddtemp-hash)
      (puthash (concat "c/f-" i) (cadddr data) hddtemp-hash)
      (setq data (cddddr data)))))

(defun hddtemp-proc ()
  "Connect to the hddtemp daemon."
  (let ((proc (open-network-stream "hdt" nil "localhost" 7634)))
    (set-process-filter proc #'hddtemp-filt)))

(defun hddtemp-init ()
  "Initialise an itimer to perodically grab hd temps."
  (let ((htimer (get-itimer "hddtemp")))
    (and htimer (delete-itimer htimer))
    (start-itimer "hddtemp" #'hddtemp-proc 60 60)))

(defun hddtemp (&optional disc)
  "Display the current temperature of DISC.

Argument DISC is a numeric prefix arg, if omitted the first hard disc
temp is displayed.  Counting begins at zero."
  (interactive "p")
  (let* ((disc (if (interactive-p)
		   (or (and (eq disc 1) (null current-prefix-arg)
			    "0")
		       (and current-prefix-arg
			    (number-to-string current-prefix-arg)))
		 (if disc
		     (number-to-string disc)
		   "0")))
	 (dev (concat "dev-" disc))
	 (mod (concat "mod-" disc))
	 (tmp (concat "tmp-" disc))
	 (c/f (concat "c/f-" disc))
	 (msg (format "%s (%s): %s°%s"
		      (gethash dev hddtemp-hash)
		      (gethash mod hddtemp-hash)
		      (gethash tmp hddtemp-hash)
		      (gethash c/f hddtemp-hash))))
    (unless (gethash dev hddtemp-hash)
      (error 'invalid-argument (format "No such disc: %s" disc)))
    (if (interactive-p)
	(message msg)
      (list (gethash dev hddtemp-hash)
	    (gethash mod hddtemp-hash)
	    (gethash tmp hddtemp-hash)
	    (gethash c/f hddtemp-hash)))))


(provide 'hddtemp)

;; On-load actions
(hddtemp-proc)
(hddtemp-init)

;;; hddtemp.el ends here
