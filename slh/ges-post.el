;;; ges-post.el --- post elisp files to gnu.emacs.sources using Gnus

;; Copyright (C) 2004 Michael Schierl
;; Copyright (C) 2004 Steve Youngs

;; RCS: $Id: ges-post.el,v 0.6 2004-03-21 11:56:13+10 steve Exp $
;; Author:        Michael Schierl <schierlm-public@gmx.de>
;;                Steve Youngs <sryoungs@bigpond.net.au>
;; Maintainer:    Steve Youngs <sryoungs@bigpond.net.au>
;; Created:       <2004-03-14>
;; Last-Modified: <2004-03-21 11:50:28 (steve)>
;; Homepage:      None.  Contact maintainer for the latest version.
;; Keywords:      gnu.emacs.sources posting Gnus news
;; Version:       $Revision: 0.6 $

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
;; gnu.emacs.sources is a newsgroup to post Elisp sources to. Since
;; the job of creating such postings is quite repetitive, Emacs can
;; help you with it. After having loaded the source file just type M-x
;; ges-post-current-buffer RET and a Gnus message buffer will be
;; prepared for you containing the full source and a default subject
;; (file name, version number (if detectable) and summary line). You
;; will be asked for a group (just hit RET for gnu.emacs.sources) and
;; whether you want to send the article directly. Otherwise point is
;; placed before the first line, so you can easily add a comment there
;; if you want to.

;; you can customize `ges-post-use-mime' to send files as a MIME
;; attachment (default is no for backwards compatibility) or
;; `ges-post-gnus-plugged' if you want to start Gnus unplugged when it
;; is not started yet.

;;; History:
;; $Log: ges-post.el,v $
;; Revision 0.6  2004-03-21 11:56:13+10  steve
;; Fix a couple of bytecompiler warnings.
;;
;; Revision 0.5  2004-03-21 11:40:42+10  steve
;; First release by Steve Youngs
;;
;;   Version 0.4 was Michael's final release as maintainer.  From
;;   this point on Steve Youngs is maintaining ges-post.
;;
;;   - Switch to BSD license.
;;   - Set a Followup-To header.
;;   - Add `ges-post-file'.
;;   - A few cosmetic changes.

;; 2004-03-19 Suggestion by Reiner Steib <reiner.steib@gmx.de>
;;   - Moved defgroup "ges-post" below "gnus-message"

;; 2004-03-17 Patch by Steve Youngs <sryoungs@bigpond.net.au>
;;   - Added a defgroup "ges-post", a subgroup of "gnus-fun".
;;   - Made the defcustoms use it
;;   - Added a keybinding
;;   - Added a convenience alias `ges-post -> ges-post-current-buffer'
;;   - Added an autoload so that entering emacs-lisp-mode will load ges-post
;;   - I figure there's no harm in a bit of advertising...

;; 2004-03-16 Patch by Steve Youngs <sryoungs@bigpond.net.au>:
;;   - start Gnus automatically if needed
;;   - ask for group name
;;   - use lisp-mnt functions to determine summary and version
;;   - optionally post as a MIME attachment
;;   - send article automatically if desired

;; 2004-03-14 First "release" in gnu.emacs.sources

;;; Code:

(require 'gnus-msg)
(require 'lisp-mnt)

(eval-when-compile
  (autoload 'with-electric-help "ehelp")
  (autoload 'font-lock-fontify-buffer "font-lock" nil t))

;;; Custom
(defgroup ges-post nil
  "Customisations for ges-post."
  :prefix "ges-post-"
  :group 'gnus-message)

(defcustom ges-post-use-mime nil
  "*When non-nil post as a MIME attachment."
  :group 'ges-post
  :type 'boolean)

(defcustom ges-post-gnus-plugged t
  "*When non-nil, start Gnus in \"plugged\" mode."
  :group 'ges-post
  :type 'boolean)

(defcustom ges-post-advertise t
  "*When non-nil advertise how the post was generated.

This inserts a line at the top of the article body advertising the
fact that the post was generated with `ges-post'.  Simply set this to
`nil' to turn this feature off."
  :type 'boolean
  :group 'ges-post)

(defcustom ges-post-use-followup-to-header t
  "*When non-nil, use a Followup-To header.

Sending followups to g.e.s if frowned upon.  That group is purely for
sending source code.  For that reason it is recommended that this
variable be left at its default value."
  :type 'boolean
  :group 'ges-post)

(defcustom ges-post-default-followup 'auto
  "What to set the Followup-To header to on posts to g.e.s.

Sending followups to g.e.s is frowned upon.  That group is purely for
sending source code.  Use this variable to set an appropriate group
for followups to your posts.

The default value auto causes ges-post to use one of the groups in
`ges-post-possible-followup-groups'.  It uses the first one in that
list that you are subscribed to.  If you are not subscribe d to any of
those groups then a Followup-To header will not be set."
  :type '(choice
	  (symbol :tag "Automatic" auto)
	  (string :tag "gnu.emacs.help" :value "gnu.emacs.help")
	  (string :tag "gnu.emacs.gnus" :value "gnu.emacs.gnus")
	  (string :tag "comp.emacs.xemacs" :value "comp.emacs.xemacs")
	  (string :tag "Followups to yourself" :value "poster")
	  (string :tag "Stop it! You'll go blind." :value "gnu.emacs.sex")
	  (string :tag "Other"))
  :group 'ges-post)

;;;###autoload
(defun ges-post-version (&optional arg)
  "Return the current version info for ges-post.

With optional argument ARG, insert version info at point in the current
buffer."
  (interactive "P")
  (let (ver)
    (with-temp-buffer
      (erase-buffer)
      (insert-file (locate-library "ges-post.el"))
      (setq ver (lm-version)))
    (if (interactive-p)
	(if arg
	    (insert (format "ges-post v%s" ver))
	  (message "ges-post v%s" ver))
      ver)))

;;;###autoload
(defun ges-post-commentary ()
  "*Display the commentary section of ges-post.el."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert (lm-commentary (locate-library "ges-post.el")))
	 (goto-char (point-min))
	 (while (re-search-forward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*ges-post Commentary*"))

;;;###autoload
(defun ges-post-copyright ()
  "*Display the copyright notice for ges-post."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert-file-contents (locate-library "ges-post.el"))
	 (goto-char (point-min))
	 (re-search-forward ";;; Commentary" nil t)
	 (beginning-of-line)
	 (narrow-to-region (point-min) (point))
	 (while (re-search-backward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*ges-post Copyright Notice*"))

(defconst ges-post-advertising-blurb
  (concat
   "(Automatically generated with ges-post.el, version "
   (ges-post-version)
   ")\n\n")
  "Blowing our own trumpet.")

(defconst ges-post-possible-followup-groups '("gnu.emacs.help"
					      "gnu.emacs.gnus"
					      "comp.emacs.xemacs")
  "List of groups that are possible candidates for Followup-To header.

As used in ges-post articles.")

(defun ges-post-compute-followup-header ()
  "Compute a value for the Followup-To header.

If `ges-post-default-followup' is non-nil, use that value.  Otherwise
use the first group in `ges-post-possible-followup-groups' that you
are subscribed to.

Returns either a name of a group as a string, or `nil'."
  (let* ((possibles ges-post-possible-followup-groups)
	 (default ges-post-default-followup)
	 (method (gnus-find-method-for-group "gnu.emacs.sources"))
	 (known-groups (gnus-groups-from-server method))
	 result done)
    (if (not (eq default 'auto))
	(progn
	  (setq result default)
	  (when (and (not (string= default "poster"))
		     (not (member default known-groups)))
	    (unless (y-or-n-p (format "You are not subscribed to %s, use anyway? "
				      default))
	      (setq result nil)))
	  result)
      (while (and possibles (not done))
	(when (member (car possibles) known-groups)
	  (setq result (car possibles)
		done t))
	(setq possibles (cdr possibles)))
      result)))

(defun ges-post-current-buffer ()
  "Prepare a posting of current buffer to gnu.emacs.sources.
Point will be placed before first line so that you can add some
comments."
  (interactive)
  (let* ((mybuf (current-buffer)) 
	 (name (or (lm-get-package-name)
		   (read-string "Package name: ")))
	 (summary (or (lm-summary)
		      (read-string "Short one-line description of package: ")))
	 (version (or (lm-version)
		      (read-string "Package version: ")))
	 (shortname (file-name-sans-extension name))
	 (subject (concat shortname " " version " -- " summary)))
    ;; If Gnus isn't running, start it.
    (unless (gnus-alive-p)
      (if ges-post-gnus-plugged
	  (gnus)
	(gnus-unplugged)))
    (gnus-group-post-news 1)
    (unless (message-field-value "Newsgroups")
      (message-goto-newsgroups)
      (insert "gnu.emacs.sources"))
    (message-goto-subject)
    (insert subject)
    (when (and (ges-post-compute-followup-header)
	       (not (message-field-value "Followup-To"))
	       ges-post-use-followup-to-header)
      (message-goto-followup-to)
      (insert (format "%s" (ges-post-compute-followup-header))))
    (message-goto-body)
    (if ges-post-use-mime
	(mml-insert-empty-tag 'part 
			      'type "application/emacs-lisp" 
			      'buffer (buffer-name mybuf)
			      'disposition "inline"
			      'description name)
      (insert-buffer mybuf))
    (when ges-post-advertise
      (message-goto-body)
      (insert ges-post-advertising-blurb))
    (when (featurep 'font-lock) (font-lock-fontify-buffer))
    (if (y-or-n-p "Do you wish to add/alter anything before sending? ")
	(message-goto-body)
      (message-send-and-exit))))

(defalias 'ges-post 'ges-post-current-buffer)

;;;###autoload
(defun ges-post-file (file)
  "Post an emacs lisp file to gnu.emacs.sources via Gnus."
  (interactive "fEmacs lisp file to post to g.e.s: ")
  (let ((buf (find-file-noselect file)))
    (set-buffer buf)
    (ges-post)
    (kill-buffer buf)))

(define-key emacs-lisp-mode-map "\M-\C-g" 'ges-post-current-buffer)

;;;###autoload(add-hook 'emacs-lisp-mode-hook '(lambda () (require 'ges-post)))

(provide 'ges-post)

;;; ges-post.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 15
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
