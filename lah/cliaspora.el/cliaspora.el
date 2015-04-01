;;; cliaspora.el --- Simple Emacs-based client for cliaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: cliaspora*
;; URL: http://diale.org/cliaspora.html

;; Copyright (c) 2012 Tiago Charters de Azevedo, Christian Giménez
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A cliaspora* client for emacs

;; Save all the files in a DIR and add that DIR to `load-path'; 
;; for instance `(add-to-list 'load-path "~/emacs.el/disaspora.el/")' to your .emacs
;; Files: cliaspora.el, cliaspora-post.el  and cliaspora-stream.el 

;; ********************
;; THANKS
;; ------
;;
;; Tiago Charters de Azevedo (tca@joindiaspora.com - http://diale.org )for taking part of this! 
;; Phil Hudson (philhudson@joindiaspora.com) for helping me testing and hunting some bugs!
;; ********************

; ah, ah...

(setq max-lisp-eval-depth 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'url)
;;(require 'url-http)
;;(require 'url-vars)
;;(require 'json)
(require 'font-lock)
(require 'markdown-translator)
(require 'markdown-mode)

(require 'cliaspora-mode)
(require 'cliaspora-urls)
(require 'cliaspora-new)
(require 'cliaspora-post)
(require 'cliaspora-stream)
(require 'cliaspora-notifications)
(require 'cliaspora-aspects)
(require 'cliaspora-contacts)
(require 'cliaspora-messages)
(require 'cliaspora-main)
(require 'cliaspora-misc)
(require 'cliaspora-post-edit-mode)
(require 'cliaspora-stream-mode)

(defconst cliaspora-el-version "0.0"
  "This version of cliaspora*-el.")

(defconst cliaspora-tag "cliaspora-el"
  "This is the tag (without \"#\") where you can see the latest news of cliaspora-el!"
  )

(defgroup cliaspora nil 
  "A mode for cliaspora* stream view and posting."
  :group 'applications)

;;; User variable:

(defcustom cliaspora-posts-directory
  "~/.cliaspora/posts/"
  "Cliaspora* temp dir (abs path)."
  :type 'dir
  :group 'cliaspora)


(defcustom cliaspora-temp-directory
  "~/.cliaspora/temp/"
  "Cliaspora* temp dir (abs path)."
  :type 'dir
  :group 'cliaspora)

(defcustom cliaspora-image-directory
  "~/.cliaspora/img/"
  "Cliaspora* image dir (abs path)."
  :type 'dir
  :group 'cliaspora)

(defcustom cliaspora-image-history-file
  "~/.cliaspora/image-sended-history"
  "This file will store all the images id an information that you may need when a post fails.

If you send an image to diáspora, the server will save it until you send a post with its id. 
If posting goes wrong for some reason, and the variable `cliaspora-images-posted' has been cleared, you miss all the ids of the images resulting in sending them again.

Well, thanks to this file, you don't need to send the images again, just look at the id, add it to the variable `cliaspora-images-posted' and that's all."
  :type 'file
  :group 'cliaspora
  )

(defcustom cliaspora-show-images-by-default
  nil
  "Loads images by default at start."
  :type 'boolean
  :group 'cliaspora)


(defcustom cliaspora-show-user-avatar nil
   "Show user images beside each users entry."
   :type 'boolean
   :group 'cliaspora)


(defcustom cliaspora-username nil
  "Username to use for connecting to cliaspora.
If nil, you will be prompted."
  :type 'string
  :group 'cliaspora)

(defcustom cliaspora-password nil
  "Password to use for connecting to cliaspora.
If nil, you will be prompted."
  :type 'string
  :group 'cliaspora)

(defcustom cliaspora-entry-file-dir
  "~/public_html/cliaspora.posts/"
  "Directory where to save posts made to cliaspora*."
  :group 'cliaspora)

;; (defcustom cliaspora-data-file
;;   "~/.cliaspora"
;;   "Name of the file do save posts made to cliaspora*."
;;   :type 'file
;;   :group 'cliaspora)

(defcustom cliaspora-data-directory
  "~/.cliaspora/"
  "Directory where for saving."
  :type 'file
  :group 'cliaspora)


(defcustom cliaspora-stream-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'cliaspora)


(defcustom cliaspora-post-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'cliaspora)

(defcustom cliaspora-single-message-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'cliaspora)

;;; Internal Variables:


(defvar cliaspora-auth-token nil
  "Authenticity token variable name.")

(defvar cliaspora-post-buffer "*cliaspora post*"
  "The name of the cliaspora post buffer.")

;;; User Functions:

(defun cliaspora ()
  "Make all dirs if they don' exist and set `cliaspora-username' 
and  `cliaspora-password' if they weren't."
  (interactive)
  (cliaspora-make-dirs)
  (cliaspora-ask)
  (cliaspora-new-session)
  (cliaspora-main))

(defun cliaspora-customize ()
  "Run Customization utility for customize D*.el."
  (interactive)
  (customize-group "cliaspora"))

(defun cliaspora-login ()
  "Ask for username and password interactivelly. 

Use `cliaspora-ask'."
  (interactive)
  (cliaspora-ask t))
  
(defun cliaspora-make-dirs ()
  "Make all dirs if they don' exist."
  (unless (file-exists-p cliaspora-data-directory)    
    (make-directory cliaspora-data-directory))
  (unless (file-exists-p cliaspora-temp-directory)
    (make-directory cliaspora-temp-directory))
  (unless (file-exists-p cliaspora-posts-directory)
    (make-directory cliaspora-posts-directory))
  (unless (file-exists-p cliaspora-image-directory)
    (make-directory cliaspora-image-directory)))

(defun cliaspora-clean-cache ()
  (interactive)
  (shell-command (concat "rm -f " cliaspora-image-directory "*")))


(defun cliaspora-ask (&optional opt)
  "Ask for username and password if `cliaspora-username' 
and  `cliaspora-password' has not been setted. `opt' t forces setting."
  (unless (and cliaspora-username cliaspora-password (null opt))
      ;; Cliaspora username and password was not setted.
    (list
     (setq cliaspora-username (read-string "username: "
					  cliaspora-username
					  nil nil))
     (setq cliaspora-password (read-passwd "password: ")))))



(defun cliaspora-new-session ()
  (let ((cliaspora-result 
         (shell-command-to-string 
          (concat "cliaspora session new " 
                  cliaspora-username "@" cliaspora-pod " " 
                  cliaspora-password))))))	   
	 

(defun cliaspora-close-session
  (shell-command (concat "cliaspora session close")))


(defun cliaspora-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "cliaspora.el, version %s" cliaspora-el-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cliaspora-hide-markdown ()
  "Hide markdown codes leaving only markdown text with colors and face properties."
  (interactive)
  (let ((inhibit-read-only t))
    (markdown-trans-apply)
    (markdown-trans-hide)))

(defun cliaspora-show-markdown ()
  "Show markdown codes."
  (interactive)
  (let ((inhibit-read-only t))
    (markdown-trans-show)))


(defun cliaspora-remove-bad-chars ()
  "Remove characters that looks ugly."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (replace-string "" "")
      (goto-char (point-min))
      (replace-string "\\[char46]" "\.")
      (goto-char (point-min))
      (replace-string "\\'" "'")
      (goto-char (point-min))
      (while (search-forward-regexp  "\\\\fB\\|\\\\fP" nil t)
        (replace-match "*" nil nil nil ))

      (goto-char (point-min))
      (while (search-forward-regexp  "^\\.PGNH$" nil t)
        (replace-match "* * * * * * * * \n" nil nil nil ))

      (goto-char (point-min))
      (while (search-forward-regexp  "^\\.br$\\|^\\.rj.*$\\|^\\.in.*$" nil t)
        (replace-match "" nil nil nil )))))




(defun cliaspora-replace-bad-links ()
  "Replace links like \"[](http://something.com\" or \"![](http://something.com/image.png\". 

This links when parsing doesn't show anything because there is no label. 
Change it to something like: \"[nolabel](http://...)\"."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp cliaspora-regexp-image nil t)
      (if (string= (match-string-no-properties 1) "[]")
	  (replace-match "[nolabel]" nil nil nil 1)))))

(defun cliaspora-get-username (username-at-pod)
  "Get only the username from the string \"username@pod\"."
  (car (split-string username-at-pod "@" t)))


(defun cliaspora-get-pod (username-at-pod)
  "Get only the username from the string \"username@pod\"."
  (cadr (split-string username-at-pod "@" t)))

(defcustom cliaspora-pod-list
  '("joindiaspora.com" "diasp.org" "calispora.org" "despora.de" "diasp.be"
    "diasp.de" "diasp.eu" "diasp0ra.ca" "diaspora.cloudid.net" "diaspora.compadre.dk"
    "diaspora.digitalinsanity.de" "diaspora.eigenlab.org" "diaspora.podzimek.org"
    "diaspora.re" "diaspora.sceal.ie" "diasporaserbia.org" "diaspora.subsignal.org"
    "diaspora.urbanabydos.ca" "dipod.org" "dipod.es" "foobar.cx" "free-beer.ch"
    "hasst-euch-alle.de" "li-la.de:3000" "londondiaspora.org" "my-seed.com" 
    "nerdpol.ch" "www.nomed.de" "ottospora.nl" "pod.geraspora.de" "poddery.com"
    "privit.us" "soc.ragriz.net" "spargo.me" "spora.com.ua" "the.diasperse.com"
    "wk3.org" "yaspora.com" "diaspora.danapriesing.net" "diasp.3towers.de"
    "d-aspora.ru" "diaspora.erchache2000.es"
    )
  "List of Pods used by `cliaspora-set-pod' function."
  :group 'cliaspora
  :type '(repeat string))

(defun cliaspora-set-pod (other-pod)
  "Change the pod for the next connection. This doesn't set it permanently, just temporary.

This command is usefull when you want to change the POD for now."
  (interactive 
   (let ((string (completing-read "POD URL?" cliaspora-pod-list nil nil)))
     (list string)))
  (setq cliaspora-pod other-pod)
  (message (format "%s %s" "Setted temporary to " other-pod)))


(provide 'cliaspora)

;;; cliaspora.el ends here.

