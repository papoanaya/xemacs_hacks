;; lj.el --- LiveJournal meets SXEmacs   -*- Emacs-Lisp -*-

;; Copyright (C) 2008 - 2011 Steve Youngs

;; Author:     Steve Youngs <steve@sxemacs.org>
;; Maintainer: Steve Youngs <steve@sxemacs.org>
;; Created:    <2008-06-15>
;; Based On:   jwz-lj.el by Jamie Zawinski <jwz@jwz.org>
;; Keywords:   blog, lj, livejournal
;; Homepage:   <http://www.sxemacs.org/lj.el>

;; This file is part of SLH (Steve's Lisp Hacks).

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
;;    First up, let me say that this would not have been possible if
;;    it weren't for JWZ's jwz-lj.el.  In fact, large portions of
;;    lj.el were lifted directly from jwz-lj.el.  So, thank you very
;;    much, Jamie!
;;
;;; *** IMPORTANT BIT ***
;;
;;    You MUST compose your LJ posts in raw HTML (XHTML 1.0 Transitional
;;    if you plan on validating the markup before you submit the post).
;;    Don't bitch and complain about how hard or inconvenient that is.
;;    I'm not listening.  The buffer where you write your posts is in a
;;    derivative of html-mode so you've got everything you need right at
;;    your fingertips.  And anyway, writing your LJ posts in raw HTML
;;    gives you much more control over what the finished post will look
;;    like.
;;
;;    Another important step before you can use lj.el is to log into
;;    your LiveJournal a/c with your web browser and check the "remember
;;    me" box on the login page.  This is the only way to have your
;;    password stored in your cookies.  Don't worry, it is encrypted.
;;
;;; A Note about validating your markup:
;;
;;    Before you can validate your posts (with psgml) you need to
;;    ensure that...
;;
;;        o You have both, the psgml, and psgml-dtds XEmacs packages
;;          installed.  Well, duh!
;;
;;        o You have a working sgml toolchain.  It is way beyond the
;;          scope of this lib (or my patience) to show you how.
;;          Instead, do what the pros do and see:
;;
;;          http://www.linuxfromscratch.org/blfs/view/svn/pst/sgml.html
;;
;;        o psgml needs to know where your catalog files are.  The
;;          easiest way to do that is to set SGML_CATALOG_FILES in
;;          your shell environment...
;;
;;          export SGML_CATALOG_FILES=/etc/sgml/catalog:/path/to/psgml-dtds/CATALOG
;;
;;; Install/Set Up:
;;
;;    Whack this lib into your load-path somewhere and...
;;     (require 'lj)
;;     (setq lj-user-id "your_lj_id")
;;
;;    When you want to compose a new LJ entry... M-x lj RET
;;
;;    There's nothing hard or overly complicated here.  Take a look at
;;    describe-mode (`C-h m') which will show you the keybindings
;;    available.  All of the "lj-mode specific" interactive commands
;;    have a binding.  There are 5 "global" commands that don't...
;;
;;         #'lj
;;         #'lj-blog-buffer
;;         #'lj-blog-region
;;         #'lj-edit-old-post
;;         #'lj-delete-old-post
;;
;;    The only reason they don't have keybindings is that I think it'd
;;    be bad form on my part to set global keys for you.  Assign them
;;    to keys if you want.
;;
;;    All of the "headers" have completion too.  A couple of tips
;;    about the completion...
;;
;;      - By default iso-left-tab (that's shift-tab for the clueless)
;;        will cycle backwards.
;;
;;      - The trick to getting multiple tags is to type a comma (`,')
;;        plus the first letter or two of the next tag you wanna use
;;        after the last inserted tag.
;;
;;; Twitter: (broken, don't use)
;;
;;    This is currently broken because twitter turned off Basic Auth
;;
;;    You can optionally post the subject header of your blog entry as
;;    a status update to Twitter, along with a URL to the entry on
;;    livejournal.com.  To do so, you must set...
;;
;;        `lj-twitter-flag'
;;        `lj-twitter-username'
;;        `lj-twitter-password'
;;
;;    The down side to this is that your twitter username and password
;;    are stored in clear text.  I'll work on a way to make that safer
;;    later.
;;
;;    Have fun with it!

;;; Todo:
;;
;;    o Make "Writer's Block" a bit friendlier.  Add the ability to
;;      choose different qotd's after one has been selected.  Also, be
;;      able to view older qotd's.
;;
;;    o Rewrite the twitter code to use OAuth.
;;

;;; Bugs:
;;
;;    I've tried to make this compatible with XEmacs 21.5 and 21.4,
;;    but I don't have either of those installed so I'm not 100%
;;    certain.  As for GNU/Emacs... absolutely no idea, but I'd doubt
;;    that this is anywhere near compatible.
;;

;;; Version:
(defconst lj-version 1.30
  "Version number of SXEmacs/LJ.")

;;; Code:
(eval-when-compile
  (autoload #'html-mode "psgml-html" nil t)
  (autoload #'executable-find "executable")
  (autoload #'completing-read-multiple "crm")
  (autoload #'sgml-indent-or-tab "psgml" nil t)
  (autoload #'sgml-parse-prolog "psgml-parse" nil t)
  (autoload #'sgml-validate "psgml" nil t)
  (autoload #'sgml-default-validate-command "psgml")
  (autoload #'browse-url-of-buffer "browse-url" nil t)
  (autoload #'customize-apropos "cus-edit" nil t)
  (autoload #'customize-group "cus-edit" nil t)
  (autoload #'regexp-opt "regexp-opt")
  (autoload #'sqlite-open "ffi-sqlite")
  (autoload #'sqlite-rows "ffi-sqlite")
  (autoload #'sqlite-close "ffi-sqlite")
  (autoload #'url-cookie-retrieve "url-cookie")
  (autoload #'url-cookie-name "url-cookie")
  (autoload #'url-cookie-value "url-cookie")
  (defvar sxemacs-codename)
  (defvar xemacs-codename)
  (defvar url-cookie-secure-storage)
  (defvar url-cookie-file "cookie"))

(eval-and-compile
  (require 'hm--html-configuration)
  (require 'psgml-html)
  (require 'font-lock)
  (unless (fboundp #'when-fboundp)
    (require 'bytedecl))
  (autoload #'mm-url-insert "mm-url"))

(defgroup lj nil
  "LiveJournal"
  :prefix "lj-"
  :link '(url-link "http://www.livejournal.com/")
  :group 'hypermedia)

(defgroup lj-twitter nil
  "LiveJournal meets Twitter"
  :prefix "lj-twitter-"
  :link '(url-link "http://www.livejournal.com/")
  :link '(url-link "http://twitter.com/")
  :group 'lj)

(defun lj-customise-faces ()
  "Customise the lj.el faces."
  (interactive)
  (customize-apropos "^lj-" 'faces))

(defun lj-customise-group ()
  "Customise lj.el user options."
  (interactive)
  (customize-group "lj"))

(defcustom lj-self-promote t
  "When non-nil, a \"Posted via...\" byline is added to a post.

The text is a single line in small print (8pt) right justified at the
very end of your post.  It should be quite inconspicuous, but you are
welcome to turn this off if you are too bashful to let the world know
what software you use."
  :type 'boolean
  :group 'lj)

(defcustom lj-user-id (user-login-name)
  "*Your LJ user ID."
  :type 'string
  :group 'lj)

(defcustom lj-signature nil
  "A signature to add to the bottom of a post.

This is analogous to an email signature.  Set this any HTML marked up
text you like, or something that returns such.  Remember to use valid
XHTML 1.0 Transitional if you plan to validate before posting."
  :type 'sexp
  :group 'lj)

(defcustom lj-cookie-flavour 'auto
  "*The default cookie flavour \(browser\) to search for cookies."
  :type '(choice
	  (symbol :tag "Automatic" :value auto)
	  (symbol :tag "Chrome" :value chrome)
	  (symbol :tag "Firefox" :value firefox)
	  (symbol :tag "Seamonkey" :value seamonkey)
	  (symbol :tag "Mozilla" :value mozilla)
	  (symbol :tag "Galeon" :value galeon)
	  (symbol :tag "Safari" :value safari)
	  (symbol :tag "Netscape" :value netscape)
	  (symbol :tag "Midori" :value midori)
	  (symbol :tag "Emacs-W3" :value w3))
  :group 'lj)

(defcustom lj-default-security-level "public"
  "*The default security level LJ posts will have."
  :type '(choice
	  (string :tag "Public" :value "public")
	  (string :tag "Private" :value "private")
	  (string :tag "All Friends" :value "usemask")
	  (string :tag "Group..."))
  :group 'lj)

(defcustom lj-directory (paths-construct-path
			 (list (user-home-directory) ".lj"))
  "*Directory for storing tags and archiving posts."
  :type 'directory
  :group 'lj)

(defcustom lj-tags-file (expand-file-name "ljtags" lj-directory)
  "*File to store list of LJ tags."
  :type 'file
  :group 'lj)

(defcustom lj-groups-file (expand-file-name "ljgrps" lj-directory)
  "*File to store list of LJ friends groups."
  :type 'file
  :group 'lj)

(defcustom lj-moods-file (expand-file-name "ljmoods" lj-directory)
  "*File to store list of LJ \"moods\"."
  :type 'file
  :group 'lj)

(defcustom lj-pickws-file (expand-file-name "pickws" lj-directory)
  "*File to store list of LJ user picture keywords."
  :type 'file
  :group 'lj)

(defcustom lj-userpic-directory
  (file-name-as-directory
   (expand-file-name "images" lj-directory))
  "*Directory to store LJ userpic files."
  :type 'directory
  :group 'lj)

(defcustom lj-drafts-directory
  (file-name-as-directory
   (expand-file-name "drafts" lj-directory))
  "*Directory where post drafts are stored."
  :type 'directory
  :group 'lj)

(defvar lj-tags nil
  "A list of LJ tags.")

(defvar lj-groups nil
  "A list of LJ friends groups.")

(defvar lj-moods nil
  "LiveJournal \"moods\".")

(defvar lj-pickws nil
  "A list of LJ userpic keywords.")

(defvar lj-default-pickw nil
  "The default LJ userpic keyword.")

;; See mpd.el in the same repo as lj.el
(defvar **mpd-var-Title* nil)
(defvar **mpd-var-Artist* nil)
(defun lj-music-mpd ()
  "Return the current song title/artist from mpd."
  (let ((song (if **mpd-var-Title*
		  (format "%s --- [%s]"
			  **mpd-var-Title*
			  **mpd-var-Artist*)
		"The Sounds of Silence --- [Marcel Marceau]")))
    song))

(defcustom lj-music (and (featurep 'mpd) #'lj-music-mpd)
  "*A function to retrieve current song for LJ music header.
This function should return a formatted string, or nil."
  :type 'function
  :group 'lj)

(defcustom lj-archive-posts t
  "*Keep an archive copy of LJ posts when non-nil."
  :type 'boolean
  :group 'lj)

(defcustom lj-archive-directory
  (file-name-as-directory
   (expand-file-name "archive" lj-directory))
  "*Directory where LJ posts are archived."
  :type 'directory
  :group 'lj)

(defcustom lj-bcc-address nil
  "*Email address to send a copy of LJ posts to.
Set to nil to disable."
  :type 'sexp
  :group 'lj)

(defcustom lj-default-location nil
  "*Default for the Location header."
  :type 'sexp
  :group 'lj)

(defcustom lj-before-preview-hook nil
  "*Hook run before previewing a post."
  :type 'hook
  :group 'lj)

(defcustom lj-after-preview-hook nil
  "*Hook run as the last thing from `lj-preview'."
  :type 'hook
  :group 'lj)

(defcustom lj-before-validate-hook nil
  "*Hook run before validating a post."
  :type 'hook
  :group 'lj)

(defcustom lj-after-validate-hook nil
  "*Hook run as the last thing from `lj-validate'."
  :type 'hook
  :group 'lj)

(defcustom lj-init-hook nil
  "*Hook run before anything else is done when starting lj."
  :type 'hook
  :group 'lj)

(defcustom lj-before-post-hook nil
  "*Hook run before posting."
  :type 'hook
  :group 'lj)

(defcustom lj-after-post-hook nil
  "*Hook run after posting."
  :type 'hook
  :group 'lj)

(defcustom lj-cut-hook nil
  "*Hooks run after inserting an LJ-CUT."
  :type 'hook
  :group 'lj)

(defcustom lj-poll-hook nil
  "*Hooks run after inserting a LJ Poll."
  :type 'hook
  :group 'lj)

(defcustom lj-journal-hook nil
  "*Hooks run after inserting a LJ Journal link."
  :type 'hook
  :group 'lj)

(defcustom lj-youtube-hook nil
  "*Hooks run after inserting a youtube/google video."
  :type 'hook
  :group 'lj)

(defcustom lj-twitter-flag nil
  "*Non-nil means to update your twitter status.

The subject header and a URL to the last blog entry is posted to
twitter as a status update if this is set."
  :type 'boolean
  :group 'lj-twitter)

(defcustom lj-twitter-username (user-login-name)
  "*Your twitter username."
  :type 'string
  :group 'lj-twitter)

;;; FIXME: Can we store this in something OTHER than plain text.  Do
;;; we even need it at all with OAuth?
(defcustom lj-twitter-password "secret"
  "*Your twitter password."
  :type 'string
  :group 'lj-twitter)

(defconst lj-clientversion
  (concat (when (featurep 'sxemacs) "S")
	  "XEmacs-"
	  emacs-program-version
	  (format "/LJ: %.2f" lj-version))
  "Client version string.")

(defconst lj-useragent
  (concat "("
	  (when (featurep 'sxemacs) "S")
	  "XEmacs/"
	  emacs-program-version
	  (format " [%s]:LJ-%.2f; steve@sxemacs.org)"
		  (if (featurep 'sxemacs)
		      sxemacs-codename
		    xemacs-codename)
		  lj-version))
  "Useragent string sent to livejournal.com.")

(defconst lj-validate-header
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
 \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">

<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <head>
    <title>LJ Post Preview</title>

    <style type=\"text/css\">
      div.ljhead {
        background: rgb(204,204,255);
        padding: 0.5em;
        border: ridge;
        borderwidth: thin;
        font-family: times new roman, verdana, helvetica, sans-serif;
        font-size: 12pt;
        font-weight: bold;
      }
      div.lj {
        background: rgb(255,235,205);
        padding: 0.5em;
        border: none;
      }
      div.ljpoll {
        color: red;
        font-weight: bold;
      }
      div.ljcut {
        background: white;
        padding: 0.5em;
        border: solid;
        borderwidth: thin;
      }
    </style>

  </head>
  <body>

"
  "Header used to construct HTML doc for previewing and validating LJ posts.")

(defconst lj-validate-footer
  "
  </body>
</html>

<!-- Leave this comment at the end of this file
Local variables:
sgml-validate-command:\"onsgmls -E0 -wall -wno-unused-param -wfully-tagged -wfully-declared -wtype-valid -wintegral -s %s %s\"
sgml-omittag:nil
sgml-shorttag:nil
sgml-namecase-general:nil
sgml-general-insert-case:lower
sgml-minimize-attributes:nil
sgml-always-quote-attributes:t
sgml-indent-step:2
sgml-indent-data:t
sgml-parent-document:nil
sgml-exposed-tags:nil
sgml-local-catalogs:nil
sgml-local-ecat-files:nil
End:
-->
"
  "Footer used to construct HTML doc for previewing and validating LJ posts.")

(defconst lj-base-url
  "http://www.livejournal.com/interface/flat"
  "The base URL where LJ posts are submitted etc.")

(defvar lj-last-entry-btime nil
  "The date/time of the last posted entry as a big integer.")

(defun lj-parse-time-string (string)
  "Parse a time STRING of the format \"YYYY-MM-DD HH:MM:SS\".

The seconds field can be ommitted and in that case 0 is used.

Returns a list suitable for passing to `encode-time' or `encode-btime'."
  (let ((regexp (concat "^\\([12][0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\s-"
			"\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?$")))
    (if (string-match regexp string)
	(let ((year (string-to-int (substring string
					      (match-beginning 1)
					      (match-end 1))))
	      (month (string-to-int (substring string
					       (match-beginning 2)
					       (match-end 2))))
	      (day (string-to-int (substring string
					     (match-beginning 3)
					     (match-end 3))))
	      (hour (string-to-int (substring string
					      (match-beginning 4)
					      (match-end 4))))
	      (min (string-to-int (substring string
					     (match-beginning 5)
					     (match-end 5))))
	      (sec (if (eq (length string) 19)
		       (string-to-int (substring string (match-beginning 7)
						 (match-end 7)))
		     0)))
	  (unless (and (>= year 1970)
		       (<= year 2099))
	    (error 'invalid-argument year))
	  (unless (and (>= month 1)
		       (<= month 12))
	    (error 'invalid-argument month))
	  (unless (and (>= day 1)
		       (<= day 31))
	    (error 'invalid-argument day))
	  (unless (and (>= hour 0)
		       (<= hour 23))
	    (error 'invalid-argument hour))
	  (unless (and (>= min 0)
		       (<= min 59))
	    (error 'invalid-argument min))
	  (unless (and (>= sec 0)
		       (<= sec 59))
	    (error 'invalid-argument sec))
	  (list sec min hour day month year))
      (error 'invalid-argument string))))

;; Probably should set up a proper prefix
(defvar lj-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'lj-mode-map)
    (define-key map [(control ?c) (control return)] #'lj-post)
    (define-key map [(control ?c) ?F] #'lj-customise-faces)
    (define-key map [(control ?c) ?G] #'lj-customise-group)
    (define-key map [(control ?c) ?P] #'lj-insert-poll)
    (define-key map [(control ?c) ?c] #'lj-cut-region)
    (define-key map [(control ?c) ?j] #'lj-insert-journal)
    (define-key map [(control ?c) ?p] #'lj-preview)
    (define-key map [(control ?c) ?w] #'lj-writers-block)
    (define-key map [(control ?c) ?y] #'lj-insert-youtube)
    (define-key map [(control ?c) (control ?f) ?M] #'lj-goto-mood)
    (define-key map [(control ?c) (control ?f) ?S] #'lj-goto-security)
    (define-key map [(control ?c) (control ?f) ?b] #'lj-goto-bcc)
    (define-key map [(control ?c) (control ?f) ?c] #'lj-goto-community)
    (define-key map [(control ?c) (control ?f) ?f] #'lj-goto-fcc)
    (define-key map [(control ?c) (control ?f) ?l] #'lj-goto-location)
    (define-key map [(control ?c) (control ?f) ?m] #'lj-goto-music)
    (define-key map [(control ?c) (control ?f) ?s] #'lj-goto-subject)
    (define-key map [(control ?c) (control ?f) ?t] #'lj-goto-tags)
    (define-key map [(control ?c) (control ?f) ?u] #'lj-goto-userpic)
    (define-key map [(control ?c) (control ?b)] #'lj-goto-body)
    (define-key map [(control meta ?v)] #'lj-validate)
    (define-key map [tab] #'lj-sgml-indent-tab-or-complete)
    (define-key map [iso-left-tab] #'lj-complete-header-backwards)
    map))

(defvar lj-header-separator "--text follows this line--"
  "Text to denote the end of the headers and beginning of the message.")

;; Faces (defaults are probably crap for a light background)
(defun lj-face-p (face)
  "Call facep on FACE."
  (facep (find-face face)))

(make-face 'lj-header-name "Face used for LJ headers.")
(set-face-parent 'lj-header-name (or (and (lj-face-p 'message-header-name)
					  'message-header-name)
				     (and (lj-face-p 'message-header-name-face)
					  'message-header-name-face)
				     'bold))

(make-face 'lj-header-subject "Face used for LJ Subject header content.")
(set-face-parent 'lj-header-subject
		 (or (and (lj-face-p 'message-header-subject)
			  'message-header-subject)
		     (and (lj-face-p 'message-header-subject-face)
			  'message-header-subject-face)
		     'default))

(make-face 'lj-header-fcc "Face used for LJ FCC header content.")
(set-face-parent 'lj-header-fcc 'font-lock-comment-face)
(make-face 'lj-header-bcc "Face used for LJ BCC header content.")
(set-face-parent 'lj-header-bcc (or (and (lj-face-p 'message-header-cc)
					 'message-header-cc)
				    (and (lj-face-p 'message-header-cc-face)
					 'message-header-cc-face)
				    'lj-header-fcc))

(make-face 'lj-header-security "Face used for LJ Security header content.")
(set-face-parent 'lj-header-security 'font-lock-warning-face)

(make-face 'lj-header-music "Face used for LJ Music header content.")
(set-face-parent 'lj-header-music
		 (or (and (lj-face-p 'message-header-xheader)
			  'message-header-xheader)
		     (and (lj-face-p 'message-header-xheader-face)
			  'message-header-xheader-face)
		     'font-lock-builtin-face))

(make-face 'lj-header-mood "Face used for LJ Mood header content.")
(set-face-parent 'lj-header-mood
		 (or (and (lj-face-p 'message-header-other)
			  'message-header-other)
		     (and (lj-face-p 'message-header-other-face)
			  'message-header-other-face)
		     'font-lock-function-name-face))

(make-face 'lj-header-userpic "Face used for LJ Userpic header content.")
(set-face-parent 'lj-header-userpic
		 (or (and (lj-face-p 'message-header-other)
			  'message-header-other)
		     (and (lj-face-p 'message-header-other-face)
			  'message-header-other-face)
		     'font-lock-function-name-face))

(make-face 'lj-header-tags "Face used for LJ Tags header content.")
(set-face-parent 'lj-header-tags
		 (or (and (lj-face-p 'message-header-newsgroups)
			  'message-header-newsgroups)
		     (and (lj-face-p 'message-header-newsgroups-face)
			  'message-header-newsgroups-face)
		     'font-lock-keyword-face))

(make-face 'lj-header-community "Face used for LJ Community header content.")
(set-face-parent 'lj-header-community 'lj-header-userpic)

(make-face 'lj-header-location "Face used for LJ Location header content.")
(set-face-parent 'lj-header-location 'lj-header-userpic)

(make-face 'lj-separator "Face used for the LJ header separator.")
(copy-face 'bold 'lj-separator)
(set-face-foreground 'lj-separator "red")

(make-face 'lj-header-itemid "Face used for LJ ItemID header content.")
(set-face-parent 'lj-header-itemid 'lj-separator)

(make-face 'lj-header-url "Face used for LJ URL header content.")
(set-face-parent 'lj-header-url 'lj-separator)

;; compatibility hoohar
(unless (featurep 'sxemacs)
  (fset #'defregexp #'defvar))

(defun lj-utf-emacs-p ()
  "Return non-nil if this S?XEmacs has utf-8 coding-system."
  (and (featurep 'mule)
       (declare-fboundp (find-coding-system 'utf-8))))

(defregexp lj-header-regexp
  (let ((headers '("Subject" "FCC" "BCC" "Security" "Community"
		   "Location" "Mood" "Music" "Userpic" "Tags"
		   "X-LJ-URL" "X-LJ-ItemID")))
    (concat (regexp-opt headers t) ":"))
  "Regular expression matching LJ headers.")

(defvar lj-font-lock-keywords
  (append
   `((,lj-header-regexp 0 lj-header-name)
     ("^Subject: \\(.*$\\)" 1 lj-header-subject)
     ("^FCC: \\(.*$\\)" 1 lj-header-fcc)
     ("^BCC: \\(.*$\\)" 1 lj-header-bcc)
     ("^Security: \\(.*$\\)" 1 lj-header-security)
     ("^Community: \\(.*$\\)" 1 lj-header-community)
     ("^Music: \\(.*$\\)" 1 lj-header-music)
     ("^Mood: \\(.*$\\)" 1 lj-header-mood)
     ("^Location: \\(.*$\\)" 1 lj-header-location)
     ("^Userpic: \\(.*$\\)" 1 lj-header-userpic)
     ("^Tags: \\(.*$\\)" 1 lj-header-tags)
     ("^X-LJ-URL: \\(.*$\\)" 1 lj-header-url)
     ("^X-LJ-ItemID: \\(.*$\\)" 1 lj-header-itemid)
     (,(regexp-quote lj-header-separator) 0 lj-separator))
   hm--html-font-lock-keywords
   html-font-lock-keywords)
  "Font lock keywords for `lj-mode'.")

;; kill/yank'd from jwz-lj.el
(defconst lj-entity-table
  '(("iexcl"  . ?\¡) ("cent"   . ?\¢) ("pound"  . ?\£) ("euro"   . ?\~)
    ("curren" . ?\¤) ("yen"    . ?\¥) ("brvbar" . ?\¦) ("sect"   . ?\§)
    ("uml"    . ?\¨) ("copy"   . ?\©) ("ordf"   . ?\ª) ("laquo"  . ?\«)
    ("not"    . ?\¬) ("shy"    . ?\­) ("reg"    . ?\®) ("macr"   . ?\¯)
    ("deg"    . ?\°) ("plusmn" . ?\±) ("sup2"   . ?\²) ("sup3"   . ?\³)
    ("acute"  . ?\´) ("micro"  . ?\µ) ("para"   . ?\¶) ("middot" . ?\·)
    ("cedil"  . ?\¸) ("sup1"   . ?\¹) ("ordm"   . ?\º) ("raquo"  . ?\»)
    ("frac14" . ?\¼) ("frac12" . ?\½) ("frac34" . ?\¾) ("iquest" . ?\¿)
    ("Agrave" . ?\À) ("Aacute" . ?\Á) ("Acirc"  . ?\Â) ("Atilde" . ?\Ã)
    ("Auml"   . ?\Ä) ("Aring"  . ?\Å) ("AElig"  . ?\Æ) ("Ccedil" . ?\Ç)
    ("Egrave" . ?\È) ("Eacute" . ?\É) ("Ecirc"  . ?\Ê) ("Euml"   . ?\Ë)
    ("Igrave" . ?\Ì) ("Iacute" . ?\Í) ("Icirc"  . ?\Î) ("Iuml"   . ?\Ï)
    ("ETH"    . ?\Ð) ("Ntilde" . ?\Ñ) ("Ograve" . ?\Ò) ("Oacute" . ?\Ó)
    ("Ocirc"  . ?\Ô) ("Otilde" . ?\Õ) ("Ouml"   . ?\Ö) ("times"  . ?\×)
    ("Oslash" . ?\Ø) ("Ugrave" . ?\Ù) ("Uacute" . ?\Ú) ("Ucirc"  . ?\Û)
    ("Uuml"   . ?\Ü) ("Yacute" . ?\Ý) ("THORN"  . ?\Þ) ("szlig"  . ?\ß)
    ("agrave" . ?\à) ("aacute" . ?\á) ("acirc"  . ?\â) ("atilde" . ?\ã)
    ("auml"   . ?\ä) ("aring"  . ?\å) ("aelig"  . ?\æ) ("ccedil" . ?\ç)
    ("egrave" . ?\è) ("eacute" . ?\é) ("ecirc"  . ?\ê) ("euml"   . ?\ë)
    ("igrave" . ?\ì) ("iacute" . ?\í) ("icirc"  . ?\î) ("iuml"   . ?\ï)
    ("eth"    . ?\ð) ("ntilde" . ?\ñ) ("ograve" . ?\ò) ("oacute" . ?\ó)
    ("ocirc"  . ?\ô) ("otilde" . ?\õ) ("ouml"   . ?\ö) ("divide" . ?\÷)
    ("oslash" . ?\ø) ("ugrave" . ?\ù) ("uacute" . ?\ú) ("ucirc"  . ?\û)
    ("uuml"   . ?\ü) ("yacute" . ?\ý) ("thorn"  . ?\þ) ("yuml"   . ?\ÿ)
    ("plusmn" . ?\±))
  "HTML entities to Latin1 characters.")

;; adapted from jwz-lj.el
(defun lj-entify-region (beg end)
  "Convert non-ASCII chars in the region BEG - END to HTML entities."
  (let ((regex (if (featurep 'sxemacs)
		   "[^[:ascii:]]"
		 ;; ho-hum, life would be simpler if XEmacs enabled
		 ;; char classes
		 (concat "[" (mapconcat
			      #'(lambda (c)
				  (make-string 1 (cdr c)))
			      lj-entity-table nil)
			 "]")))
	(case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (setq end (copy-marker end))
      (while (re-search-forward regex end t)
	(let* ((char (preceding-char))
	       (entity (or (car (rassq char lj-entity-table))
			   (error "No entity %c" char))))
	  (delete-char -1)
	  (insert-before-markers "&" entity ";")))))
  (and-fboundp #'charsets-in-region
    (delq 'ascii (charsets-in-region beg end))
    (error "Non-ASCII characters exist in this buffer")))

(defconst lj-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
       ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
       ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

(defun lj-hexify-string (str &optional http-entify)
  "Escape characters STR so STR can be used in a URL.

With non-nil HTTP-ENTIFY, convert non-ASCII characters to HTTP
entities."
  (with-temp-buffer
    (insert str)
    (and http-entify
	 (lj-entify-region (point-min) (point-max)))
    (mapconcat
     #'(lambda (char)
	 (if (not (memq char lj-unreserved-chars))
	     (if (< char 16)
		 (format "%%0%X" char)
	       (if (> char 255)
		   (error "Hexifying multibyte character %s" str))
	       (format "%%%X" char))
	   (char-to-string char)))
     (buffer-string) "")))

(when-fboundp #'ffi-defun
  (ignore-errors
    (require 'ffi-sqlite)
    (require 'ffi-curl)))

;; adapted from jwz-lj.el
(defun lj-extract-sql-cookies (file chromep)
  "Extract LJ cookie data from SQL cookies FILE.

Non-nil CHROMEP forces a Google Chrome compatible sql query."
  (let ((sql (if chromep
		 ;; chrome sql cookies
		 (concat "SELECT name,value FROM cookies "
			 "WHERE host_key=\".www.livejournal.com\" "
			 "OR host_key=\".livejournal.com\"")
	       ;; mozilla based sql cookies
	       (concat "SELECT name,value FROM moz_cookies "
		       "WHERE host=\".www.livejournal.com\" "
		       "OR host=\".livejournal.com\""))))
    (if (featurep 'ffi-sqlite)
	;; Try SXEmacs' sexy ffi-sqlite if it's available
	(let* ((db (sqlite-open file))
	       (rows (sqlite-rows db sql)))
	  (sqlite-close db)
	  (when (listp rows)
	    (concat "Cookie: "
		    (mapconcat
		     #'(lambda (c)
			 (concat (car c) "=" (cadr c)))
		     rows "; ")
		    "\r\n")))
      ;; The old fashioned way
      (unless (executable-find "sqlite3")
	(error "Can't find sqlite3"))
      (let* ((sql (shell-command-to-string
		   (concat "sqlite3 " file "'" sql ";'")))
	     (slist (butlast
		     (split-string-by-char
		      (replace-regexp-in-string "\n" "|" sql) ?|)))
	     cookies)
	(while slist
	  (push (cons (car slist) (cadr slist)) cookies)
	  (setq slist (cddr slist)))
	(when cookies
	  (concat "Cookie: "
		  (mapconcat
		   #'(lambda (c)
		       (concat (car c) "=" (cdr c)))
		   (reverse cookies) "; ")
		  "\r\n"))))))

;;; FIXME: redo this with #'xml-parse-file.  Just as soon as I can get
;; me grubby little hands on one of these xml cookie files.
;; kill/yank'd from jwz-lj.el
;;; FIXME:  this will now be broken!
(defun lj-extract-xml-cookies (file)
  "Extract LJ data from XML cookies FILE."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (insert-file-contents file nil nil nil t)
      (goto-char (point-min))
      (search-forward "<dict>")
      (let ((result "")
            (start (point))
            end
            domain path name value)
        (while (search-forward "</dict>" nil t)
          (setq end (point))
          (goto-char start)
          (cond
           ((search-forward "livejournal.com" end t)  ; bail fast

            (goto-char start)
            (re-search-forward (concat "<key>Domain</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq domain (match-string 1))
            (goto-char start)
            (re-search-forward (concat "<key>Path</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq path (match-string 1))
            (goto-char start)
            (re-search-forward (concat "<key>Name</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq name (match-string 1))
            (goto-char start)
            (re-search-forward (concat "<key>Value</key>[ \t\n\r]*"
                                       "<string>\\([^<>]+\\)</string>")
                               end)
            (setq value (match-string 1))
            (if (string-match "\\blivejournal\\.com$" domain)
                (setq result
                      (concat domain "\tTRUE\t" path "\tFALSE\t0\t"
                              name "\t" value
                              "\n" result)))))
          (goto-char end)
          (setq start end))
        (delete-region (point-min) (point-max))
        (insert result))))
  nil)

(defun lj-extract-text-cookies (file)
  "Extract LJ data from text based cookies FILE."
  (let ((host-match "\\.livejournal\\.com$")
	cookies)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at (concat "^\\([^\t\r\n]+\\)\t"  ; 1 host
				  "\\([^\t\r\n]+\\)\t"  ; 2 bool
				  "\\([^\t\r\n]+\\)\t"  ; 3 path
				  "\\([^\t\r\n]+\\)\t"  ; 4 bool
				  "\\([^\t\r\n]+\\)\t"  ; 5 time_t
				  "\\([^\t\r\n]+\\)\t"  ; 6 key
				  "\\([^\t\r\n]+\\)$")) ; 7 val
	  (let ((host (match-string 1))
		(key (match-string 6))
		(val (match-string 7)))
	    (when (and (string-match host-match host)
		       (not (assoc key cookies)))
	      (setq cookies (cons (cons key val) cookies))))
	  (forward-line 1))))
    (when cookies
      (concat "Cookie: "
	      (mapconcat
	       #'(lambda (c)
		   (concat (car c) "=" (cdr c)))
	       (nreverse cookies) "; ")
	      "\r\n"))))

(defun lj-extract-w3-cookies ()
  "Extract LJ cookie data from Emacs-W3 cookies."
  (let* ((secure (and url-cookie-secure-storage t))
	 (w3cookies (remove-duplicates
		     (append (url-cookie-retrieve
			      "www.livejournal.com" "/" secure)
			     (url-cookie-retrieve
			      ".livejournal.com" "/" secure))
		     :test #'equal)))
    (when w3cookies
      (replace-regexp-in-string
       "; \r\n" "\r\n"
       (concat "Cookie: "
	       (mapconcat
		#'(lambda (c)
		    (unless (string-equal (url-cookie-name c) "HttpOnly")
		      (concat (url-cookie-name c) "=" (url-cookie-value c))))
		(nreverse w3cookies) "; ")
	       "\r\n")))))

;; adapted from jwz-lj.el, but rewritten from scratch
(defun lj-get-cookies (flavour)
  "Return a string of LJ cookie data suitable for HTTP POST'ing.

Argument FLAVOUR specifies which browser's cookies to check.  If it is
the symbol `auto' \(the default\) all browsers will be searched in the
following order...

Google Chrome, Firefox, SeaMonkey, Mozilla, Galeon, Safari, Nescape,
Midori, and Emacs-W3 for cookie data."
  (catch 'found
    (let (cookies)

      ;; Google Chrome
      (when (or (eq flavour 'chrome)
		(eq flavour 'auto))
	(let ((dir (paths-construct-path
		    '(".config" "google-chrome" "Default")
		    (user-home-directory))))
	  (and (file-exists-p (expand-file-name "Cookies" dir))
	       (setq cookies (lj-extract-sql-cookies
			      (expand-file-name "Cookies" dir) t))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Firefox 1-3 (works for sqlite cookies, plain text cookies are
      ;; untested)
      (when (or (eq flavour 'firefox)
		(eq flavour 'auto))
	(let ((d1 (paths-construct-path
		   '("Library" "Application Support" "Firefox" "Profiles")
		   (user-home-directory)))
	      (d2 (paths-construct-path
		   '(".mozilla" "firefox") (user-home-directory)))
	      dir)
	  (if (file-directory-p d1)
	      (setq dir (car (directory-files d1 t "\\.default$" nil 'dirs)))
	    (setq dir (car (directory-files d2 t "\\.default$" nil 'dirs))))
	  (or (and (file-exists-p (expand-file-name "cookies.txt" dir))
		   (setq cookies (lj-extract-text-cookies
				  (expand-file-name "cookies.txt" dir))))
	      (and (file-exists-p (expand-file-name "cookies.sqlite" dir))
		   (setq cookies (lj-extract-sql-cookies
				  (expand-file-name "cookies.sqlite" dir) nil)))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; SeaMonkey (untested)
      (when (or (eq flavour 'seamonkey)
		(eq flavour 'auto))
	(when (file-directory-p (expand-file-name ".mozilla/seamonkey"
						  (user-home-directory)))
	  (let ((dir (car (directory-files "~/.mozilla/seamonkey"
					   t "\\.default$" nil 'dirs))))
	    (or (and (file-exists-p (expand-file-name "cookies.txt" dir))
		     (setq cookies (lj-extract-text-cookies
				    (expand-file-name "cookies.txt" dir))))
		(and (file-exists-p (expand-file-name "cookies.sqlite" dir))
		     (setq cookies (lj-extract-sql-cookies
				    (expand-file-name "cookies.sqlite" dir) nil))))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Mozilla (untested)
      (when (or (eq flavour 'mozilla)
		(eq flavour 'auto))
	(when (file-directory-p (expand-file-name ".mozilla"
						  (user-home-directory)))
	  (let ((d1 (paths-construct-path '(".mozilla" "default")
					  (user-home-directory)))
		(d2 (paths-construct-path `(".mozilla" ,(user-login-name))
					  (user-home-directory)))
		dir)
	    (if (file-directory-p d1)
		(setq dir (car (directory-files d1 t "\\.slt$" nil 'dirs)))
	      (setq dir (car (directory-files d2 t "\\.slt$" nil 'dirs))))
	    (and (file-exists-p (expand-file-name "cookies.txt" dir))
		 (setq cookies (lj-extract-text-cookies
				(expand-file-name "cookies.txt" dir))))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Galeon (untested)
      (when (or (eq flavour 'galeon)
		(eq flavour 'auto))
	(let ((dir (paths-construct-path
		    '(".galeon" "mozilla" "galeon") (user-home-directory))))
	  (and (file-exists-p (expand-file-name "cookies.txt" dir))
	       (setq cookies (lj-extract-text-cookies
			      (expand-file-name "cookies.txt" dir)))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Safari (untested)
      (when (or (eq flavour 'safari)
		(eq flavour 'auto))
	(let ((dir (paths-construct-path '("Library" "Cookies")
					 (user-home-directory))))
	  (and (file-exists-p (expand-file-name "Cookies.plist" dir))
	       (setq cookies (lj-extract-xml-cookies
			      (expand-file-name "Cookies.plist" dir)))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Netscape (untested)
      (when (or (eq flavour 'netscape)
		(eq flavour 'auto))
	(let ((dir (paths-construct-path '(".netscape")
					 (user-home-directory))))
	  (and (file-exists-p (expand-file-name "cookies" dir))
	       (setq cookies (lj-extract-text-cookies
			      (expand-file-name "cookies" dir)))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Midori (works, but Midori doesn't take too much care about
      ;; invalid or broken cookies so YMMV here)
      (when (or (eq flavour 'midori)
		(eq flavour 'auto))
	(let ((dir (paths-construct-path '(".config" "midori")
					 (user-home-directory))))
	  (and (file-exists-p (expand-file-name "cookies.txt" dir))
	       (setq cookies (lj-extract-text-cookies
			      (expand-file-name "cookies.txt" dir)))))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Emacs-W3 URL (works with W3 in XE packages)
      (when (or (eq flavour 'w3)
		(eq flavour 'auto))
	(and (file-exists-p url-cookie-file)
	     (setq cookies (lj-extract-w3-cookies)))
	(if cookies
	    (throw 'found cookies)
	  (when (not (eq flavour 'auto))
	    (lj-get-cookies 'auto))))

      ;; Gah! no cookes anywhere!
      (error 'search-failed "LJ Cookie data"))))

(defvar lj-cookies (lj-get-cookies lj-cookie-flavour)
  "Alist of cookie data to send to LJ.")

;; adapted from jwz-lj.el
(defun lj-http-post (url cookies parser)
  "Sends a HTTP POST to URL with COOKIES.

Argument PARSER is a function to handle parsing the output received."
  (unless (string-match "\\`https?://\\([^/]+\\)\\([^?&]+\\)\\?\\(.*\\)\\'" url)
    (error "Unparsable url: %s" url))
  (let* ((host (match-string 1 url))
         (port 80)
         (path (match-string 2 url))
         (args (match-string 3 url))
         (post-cmd
          (concat "POST " path " HTTP/1.0\r\n"
                  "Content-Type: application/x-www-form-urlencoded\r\n"
                  "Content-Length: " (int-to-string (length args)) "\r\n"
                  "Host: " host "\r\n"
                  "X-LJ-Auth: cookie\r\n"
                  cookies
                  "\r\n"
                  args))
	 (buf (generate-new-buffer " *LJ-process*"))
         proc)
    (setq proc (open-network-stream "LiveJournal" buf host port))
    (when (lj-utf-emacs-p)
      (set-process-coding-system proc 'utf-8 'utf-8))
    (process-send-string proc post-cmd)
    (message "HTTP POST sent to %s" host)
    (while (eq (process-status proc) 'open)
      (unless (accept-process-output proc 60)
 	(delete-process proc)
 	(error "[LJ] Server error: timeout")))
    (funcall parser buf)))

(defun lj-proc-success ()
  "Return t when LJ processes are successful.

By \"successful\" we mean that livejournal.com didn't complain about
anything we sent it."
  (let ((regex "^success\n\\(.*$\\)")
	result)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regex nil t)
	  (setq result (match-string 1))
	(error "[LJ] Server error: try again later"))
      (cond ((string= result "OK")
	     t)
	    ((string= result "FAIL")
	     (let ((ereg "^errmsg\n\\(.*$\\)"))
	       (save-excursion
		 (goto-char (point-min))
		 (re-search-forward ereg)
		 (if (string-match "Incorrect time value" (match-string 1))
		     (lj-post 'out-of-order)
		   (error "[LJ]: %s" (match-string 1))))))
	    (t
	     (error "[LJ]: Unknown error"))))))

(defun lj-friends-proc-parser (buf)
  "Processes the output from `lj-get-friends-groups'.
Argument BUF is the process buffer used."
  (let ((regexp "^frgrp_\\([0-9]+\\)_name\n\\(.*$\\)")
	groups)
    (with-current-buffer buf
      (when (lj-proc-success)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (setq groups
		(cons (cons (match-string 2) (string-to-int (match-string 1)))
		      groups)))
	(kill-buffer nil))
      (when groups
	(or (file-directory-p lj-directory)
	    (make-directory-path lj-directory))
	(with-current-buffer (find-file-noselect lj-groups-file)
	  (erase-buffer)
	  (insert ";;; Automatically generated DO NOT EDIT -*- Emacs-Lisp -*-\n"
		  (format "(setq lj-groups (quote %S))" groups))
	  (save-buffer)
	  (eval-current-buffer nil)
	  (kill-buffer nil))))))

(defun lj-get-friends-groups ()
  "Retrieve an alist of groups/groupids from Livejournal."
  (let ((cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(url (concat lj-base-url
		     "?mode=getfriendgroups"
		     "&user=" lj-user-id
		     "&auth_method=cookie"
		     (format "&ver=%d" (if (lj-utf-emacs-p) 1 0)))))
    (lj-http-post url cookies #'lj-friends-proc-parser)))

(defun lj-tags-proc-parser (buf)
  "Process the output from `lj-get-tags'.
Argument BUF is the process buffer used."
  (let ((regexp "tag_[0-9]+_name\n\\(.*$\\)")
	tags)
    (with-current-buffer buf
      (when (lj-proc-success)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (push (match-string 1) tags))
	(kill-buffer nil))
      (when tags
	(or (file-directory-p lj-directory)
	    (make-directory-path lj-directory))
	(with-current-buffer (find-file-noselect lj-tags-file)
	  (erase-buffer)
	  (insert ";;; Automatically generated DO NOT EDIT -*- Emacs-Lisp -*-\n"
		  (format "(setq lj-tags (quote %S))" tags))
	  (save-buffer)
	  (eval-current-buffer nil)
	  (kill-buffer nil))))))

(defun lj-get-tags ()
  "Retrieve a list of defined tags from Livejournal."
  (let ((cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(url (concat lj-base-url
		     "?mode=getusertags"
		     "&user=" lj-user-id
		     "&auth_method=cookie"
		     (format "&ver=%d" (if (lj-utf-emacs-p) 1 0)))))
    (lj-http-post url cookies #'lj-tags-proc-parser)))

(defun lj-get-userpic-noffi (url file)
  "Download userpic from URL to FILE."
  (if (executable-find "curl")
      (shell-command (concat "curl " url " -so " file) nil)
    (error 'unimplemented "non-FFI leeching")))

(defun lj-get-userpics ()
  "Leech your userpics from livejournal.com."
  (unless (file-directory-p lj-userpic-directory)
    (make-directory-path lj-userpic-directory))
  (let ((pics (mapcar #'car lj-pickws)))
    (mapcar
     #'(lambda (p)
	 (unless (file-exists-p
		  (expand-file-name p lj-userpic-directory))
	   (let ((file (expand-file-name p lj-userpic-directory))
		 (url (cdr (assoc p lj-pickws))))
	     (if (featurep '(and sxemacs ffi-curl))
		 (declare-fboundp (curl:download url file))
	       (lj-get-userpic-noffi url file)))))
     pics)))

(defun lj-pickws-proc-parser (buf)
  "Process the output from `lj-get-pickws'.
Argument BUF is the process buffer used."
  (let ((msg "^message\n\\(.*$\\)")
	defaultk defaultu keywords)
    (with-current-buffer buf
      (when (lj-proc-success)
	(goto-char (point-min))
	(save-excursion
	  (re-search-forward "defaultpicurl\n\\(.*$\\)")
	  (setq defaultu (match-string 1))
	  (let ((defidx
		  (and
		   (re-search-forward (concat "^pickwurl_\\([0-9]+\\)\n"
					      defaultu) nil t)
		   (match-string 1))))
	    (goto-char (point-min))
	    (re-search-forward (concat "^pickw_" defidx "\n\\(.*$\\)") nil t)
	    (setq defaultk (match-string 1))
	    (setq keywords (cons (cons defaultk defaultu) keywords))))
	(save-excursion
	  (while (re-search-forward "^pickw_\\([0-9]+\\)\n\\(.*$\\)" nil t)
	    (let* ((key (match-string 2))
		   (url (save-excursion
			  (goto-char (point-min))
			  (and (re-search-forward
				(concat "pickwurl_"
					(match-string 1) "\n\\(.*$\\)") nil t)
			       (match-string 1)))))
	      (unless (string= key defaultk)
		(setq keywords (cons (cons key url) keywords))))))
	(when (re-search-forward msg nil t)
	  (pop-to-buffer (get-buffer-create "*LJ Message*"))
	  (insert "Important Message From LiveJournal:\n"
		  "==================================\n\n")
	  (insert (match-string 1)))
	(kill-buffer buf))
      (when keywords
	(or (file-directory-p lj-directory)
	    (make-directory-path lj-directory))
	(with-current-buffer (find-file-noselect lj-pickws-file)
	  (erase-buffer)
	  (insert ";;; Automatically generated DO NOT EDIT -*- Emacs-Lisp -*-\n"
		  (format "(setq lj-pickws (quote %S))" keywords))
	  (insert (format "\n(setq lj-default-pickw %S)" defaultk))
	  (save-buffer)
	  (eval-current-buffer nil)
	  (kill-buffer nil))))
    (lj-get-userpics)))

(defun lj-get-pickws ()
  "Retieve an alist of userpic keyword/url pairs."
  (let ((cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(url (concat lj-base-url
		     "?mode=login"
		     "&user=" lj-user-id
		     "&auth_method=cookie"
		     (format "&ver=%d" (if (lj-utf-emacs-p) 1 0))
		     "&clientversion=" lj-clientversion
		     "&getpickws=1"
		     "&getpickwurls=1")))
    (lj-http-post url cookies #'lj-pickws-proc-parser)))

(defun lj-moods-proc-parser (buf)
  "Process the output from `lj-get-moods'.
Argument BUF is the process buffer used."
  (let ((regexp "mood_[0-9]+_id\n\\(.*\\)\nmood_[0-9]+_name\n\\(.*\\)")
	(msg "^message\n\\(.*$\\)")
	moods)
    (with-current-buffer buf
      (when (lj-proc-success)
	(goto-char (point-min))
	(save-excursion
	  (while (re-search-forward regexp nil t)
	    (setq moods (cons (cons (match-string 2)
				    (string-to-int (match-string 1)))
			      moods))))
	(when (re-search-forward msg nil t)
	  (pop-to-buffer (get-buffer-create "*LJ Message*"))
	  (insert "Important Message From LiveJournal:\n"
		  "==================================\n\n")
	  (insert (match-string 1)))
	(kill-buffer buf))
      (or (file-directory-p lj-directory)
	  (make-directory-path lj-directory))
      (with-current-buffer (find-file-noselect lj-moods-file)
	(erase-buffer)
	(insert ";;; Automatically generated DO NOT EDIT -*- Emacs-Lisp -*-\n"
		(format "(setq lj-moods (quote %S))" moods))
	(save-buffer)
	(eval-current-buffer nil)
	(kill-buffer nil)))))

(defun lj-get-moods ()
  "Retieve an alist of mood/moodid pairs."
  (let ((cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(url (concat lj-base-url
		     "?mode=login"
		     "&user=" lj-user-id
		     "&auth_method=cookie"
		     (format "&ver=%d" (if (lj-utf-emacs-p) 1 0))
		     "&clientversion=" lj-clientversion
		     "&getmoods=0")))
    (lj-http-post url cookies #'lj-moods-proc-parser)))

(defvar lj-last-user-set-time nil)
(defvar lj-qotd 0)
;; adapted from jwz-lj.el
(defun lj-construct-url (subject body user
				&optional security tags community
				auto-format no-comments mood location
				music pickw date backdated itemid)
  "Construct a URL to use for posting to LiveJournal.

Argument SUBJECT, a string, which is the title of the post.
Argument BODY, a string, is the body of the post.
Argument USER, a string, is the LJ userid to post as.

Optional argument SECURITY, a string, is the security level this post
will have.  The default is `lj-default-security-level'.

Optional argument TAGS, a string, which is a comma delimited list of
tags to add to this post.

Optional argument COMMUNITY, a string, which is the name of a LJ forum
to send this post to instead of the user's blog.

Optional argument AUTO-FORMAT, when non-nil request that the LJ server
automatically formats the post.  The default is nil, which means the
post should NOT be auto formatted by LJ.

Optional argument NO-COMMENTS, when non-nil means to turn off comments
on the post.

Optional argument MOOD, a string or an integer, is the post's \"mood\"
header.  If it is an integer, it is a \"mood id\" which is mapped to a
string by LJ.

Optional argument LOCATION, a string, free-form text describing your
current location.  Livejournal turns it into a search URL to google
maps.

Optional argument MUSIC, a string, of the currently playing mp3/ogg.

Optional argument PICKW, a string, of the userpic keyword to use.  If
omitted, your default LJ userpic will be used.

Optional argument DATE, an internal time value as returned by
`encode-time'.  Used to set a date/time on a post, if omitted the
current time is used.

Optional boolean argument BACKDATED, causes the \"backdated\" flag to be
set which will prevent the post from showing up on friends pages."
  (let* ((friends-mask nil)
	 (tl (split-string-by-char
		(format-time-string "%Y,%m,%d,%H,%M"
				    (or date (current-time))) ?,))
	 (year (first tl))
	 (month (second tl))
	 (day (third tl))
	 (hour (fourth tl))
	 (minute (fifth tl))
	 (ctime (apply #'encode-btime (lj-parse-time-string
				       (format "%s-%s-%s %s:%s"
					       year month day hour minute))))
	 (ltime (or lj-last-entry-btime (and (lj-get-last-entry-btime)
					     lj-last-entry-btime)))
	 url)
    ;; save custom date in case something goes wrong (not for edits)
    (if (and date 
	     (not (zerop (length itemid))))
	(setq lj-last-user-set-time date)
      (setq lj-last-user-set-time nil))
    (setq subject (lj-hexify-string subject t))
    (setq body (lj-hexify-string body t))
    ;; security level
    (if (not (string-match "p\\(ublic\\|rivate\\)" security))
	(if (string= "usemask" security)
	    (setq friends-mask 1)
	  (let* ((groups (or lj-groups (lj-get-friends-groups)))
		 (id (cdr (assoc security groups))))
	    (if id
		(setq security "usemask"
		      friends-mask (lsh 1 id))
	      (error "Unknown friends group: %s" security)))))
    (setq security (lj-hexify-string security t))
    ;; tags
    (if (> (length tags) 0)
	(setq tags (lj-hexify-string tags t))
      (setq tags nil))
    ;; mood
    (cond ((cdr (assoc mood lj-moods))
	   (setq mood (cdr (assoc mood lj-moods))))
	  ((and (stringp mood)
		(> (length mood) 0))
	   (setq mood (lj-hexify-string mood t)))
	  ((integerp mood) nil)
	  (t
	   (setq mood nil)))
    ;; music
    (if (> (length music) 0)
	(setq music (lj-hexify-string music t))
      (setq music nil))
    ;; userpic
    (if (> (length pickw) 0)
	(setq pickw (lj-hexify-string pickw t))
      (setq pickw nil))
    ;; community
    (if (> (length community) 0)
	(setq community (lj-hexify-string community t))
      (setq community nil))
    ;; location
    (if (> (length location) 0)
	(setq location (lj-hexify-string location t))
      (setq location nil))
    ;; maybe force opt_backdated (not touching for edits)
    (when (and (> ltime ctime)
	       (not (zerop (length itemid))))
      (setq backdated t))
    ;; the final url
    (setq url (concat
	       lj-base-url
	       (format "?mode=%sevent" (if (zerop (length itemid))
					   "post"
					 "edit"))
	       "&user=" user
	       "&auth_method=cookie"
	       (format "&ver=%d" (if (lj-utf-emacs-p) 1 0))
	       "&subject=" subject
	       "&security=" security
	       (when friends-mask
		 (format "&allowmask=%d" friends-mask))
	       (when tags
		 (format "&prop_taglist=%s" tags))
	       (when community
		 (format "&usejournal=%s" community))
	       (when (zerop (length itemid)) ; leave date alone when editing
		 (format
		  "&year=%s&mon=%s&day=%s&hour=%s&min=%s"
		  year month day hour minute))
	       (when mood
		 (if (integerp mood)
		     (format "&prop_current_moodid=%d" mood)
		   (format "&prop_current_mood=%s" mood)))
	       (when music
		 (format "&prop_current_music=%s" music))
	       (when location
		 (format "&prop_current_location=%s" location))
	       (when pickw
		 (format "&prop_picture_keyword=%s" pickw))
	       (unless (zerop lj-qotd)
		 (format "&prop_qotdid=%d" lj-qotd))
	       "&prop_opt_backdated=" (if backdated "1" "0")
	       "&prop_opt_preformatted=" (if auto-format "0" "1")
	       "&prop_opt_nocomments=" (if no-comments "1" "0")
	       "&prop_useragent=" (lj-hexify-string lj-useragent)
	       (unless (zerop (length itemid))
		 (format "&itemid=%s" itemid))
	       "&event=" body))
    url))

(defun lj-cut-toggle-top ()
  "Toggle view of LJ CUT text."
  (interactive)
  (save-excursion
    (goto-char (point-at-eol))
    (forward-char 1)
    (set-extent-property
     (extent-at (point) nil 'ljcut)
     'invisible (not (extent-property
		      (extent-at (point) nil 'ljcut) 'invisible)))))


(defun lj-cut-mouse-toggle-top (event)
  "Toggle view of LJ CUT text under EVENT."
  (interactive "e")
  (let ((epoint (event-point event)))
    (save-excursion
      (goto-char epoint)
      (goto-char (point-at-eol))
      (forward-char 1)
      (set-extent-property
       (extent-at (point) nil 'ljcut)
       'invisible (not (extent-property
			(extent-at (point) nil 'ljcut) 'invisible))))))

(defun lj-cut-toggle-bottom ()
  "Toggle view of LJ CUT text."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (backward-char 1)
    (set-extent-property
     (extent-at (point) nil 'ljcut)
     'invisible (not (extent-property
		      (extent-at (point) nil 'ljcut) 'invisible)))))

(defun lj-cut-mouse-toggle-bottom (event)
  "Toggle view of LJ CUT text under EVENT."
  (interactive "e")
  (let ((epoint (event-point event)))
    (save-excursion
      (goto-char epoint)
      (goto-char (point-at-bol))
      (backward-char 1)
      (set-extent-property
       (extent-at (point) nil 'ljcut)
       'invisible (not (extent-property
			(extent-at (point) nil 'ljcut) 'invisible))))))

(defvar lj-cut-keymap-top
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'lj-cut-keymap-top)
    (define-key map [return] #'lj-cut-toggle-top)
    (define-key map [button2] #'lj-cut-mouse-toggle-top)
    map)
  "Keymap for LJ CUT extents.")

(defvar lj-cut-keymap-bottom
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'lj-cut-keymap-bottom)
    (define-key map [return] #'lj-cut-toggle-bottom)
    (define-key map [button2] #'lj-cut-mouse-toggle-bottom)
    map)
  "Keymap for LJ CUT extents.")

(defun lj-cut-region (b e)
  "Mark text in the region B to E as an LJ CUT.

The text that is to be hidden behind the LJ CUT is made invisible in
the buffer.  The visibility can be toggled with Return or Button2 on
either of the lj-cut delimiters."
  (interactive "r")
  (let ((echo "Ret / Button2 Toggle View")
	ext)
    (save-restriction
      (narrow-to-region b e)
      (lj-text-to-html (point-min) (point-max))
      (set-extent-properties
       (setq ext (make-extent (point-min) (point-max)))
       '(start-open t end-open t invisible t ljcut t))
      (goto-char (point-min))
      (set-extent-properties
       (insert-face "<lj-cut text=\"---More---\">" 'widget-button-face)
       `(help-echo ,echo
		   balloon-help ,echo
		   keymap ,lj-cut-keymap-top
		   mouse-face font-lock-warning-face))
      (insert "\n")
      (goto-char (point-max))
      (set-extent-properties
       (insert-face "</lj-cut>" 'widget-button-face)
       `(help-echo ,echo
		   balloon-help ,echo
		   keymap ,lj-cut-keymap-bottom
		   mouse-face font-lock-warning-face))
      (insert "\n"))
    (set-extent-properties
     ext
     '(start-open nil end-open nil))
    (run-hooks 'lj-cut-hook)))

(defvar lj-poll-types '("radio" "check" "drop" "text" "scale")
  "LJ poll types.")

(defun lj-insert-poll (name type question)
  "Insert a poll into a LJ post.
Argument NAME is the title of the poll.
Argument TYPE is the type of poll \(see `lj-poll-types'\).
Argument QUESTION is the poll question, or \"topic\"."
  (interactive
   (list (read-string "Poll Title: " nil nil "unnamed poll")
	 (completing-read "Poll Type (default \"radio\"): "
			  (mapcar #'list lj-poll-types)
			  nil t nil nil "radio")
	 (read-string "Poll Question: ")))
  (let ((voters (completing-read "Who can vote (default \"all\"): "
				 (mapcar #'list '("all" "friends"))
				 nil t nil nil "all"))
	(viewers (completing-read "Who can view results (default \"all\"): "
				  (mapcar #'list '("all" "friends" "none"))
				  nil t nil nil "all"))
	(p (point)))
    (insert
     (format "\n<lj-poll name=\"%s\" whovote=\"%s\" whoview=\"%s\">"
	     name voters viewers)
     (format "\n<lj-pq type=\"%s\"" type))
    (cond
      ((string= "scale" type)
       (let ((low (read-number "Scale low mark (int): " t "1"))
	     (high (read-number "Scale high mark (int): " t "10"))
	     (step (read-number "Stepping: " t "1")))
	 (insert (format " from=\"%d\" to=\"%d\" by=\"%d\">"
			 low high step)
		 (format "\n%s" question))))
      ((string= "text" type)
       (let* ((size (read-number "Text box size: " t "50"))
	      (max (read-number "Max answer length: " t
				(number-to-string (1- size)))))
	 (insert (format " size=\"%d\" maxlength=\"%d\">" size max)
		 (format "\n%s" question))))
      (t (insert (format ">\n%s" question))))
    (unless (string-match "scale\\|text" type)
      (let ((x "x"))
	(while (not (zerop (length x)))
	  (setq x (read-string "Poll Answer (RET to finish): "))
	  (or (zerop (length x))
	      (insert (format "\n<lj-pi>%s</lj-pi>" x))))))
    (insert "\n</lj-pq>"
	    "\n</lj-poll>")
    (indent-region p (point) nil)
    (run-hooks 'lj-poll-hook)))

;; Apparantly the _VALID_ markup that this function produces causes
;; some (all?) versions of M$ Internet Exploiter to buffer the entire
;; movie before beginning playback.  Hey, lets call it a FEATURE!
(defun lj-insert-youtube (url)
  "Insert a Google or Youtube video URL into a LJ post."
  (interactive "sVideo URL: ")
  (let* ((googlep (string-match "^http://video\\.google\\.com/.*$" url))
	 (youtubep (string-match "^http://\\(www\\.\\)?youtube\\.com/.*$" url))
	 (w (if googlep 420 400))
	 (h (if googlep 352 338))
	 (p (point)))
    (unless (or googlep youtubep)
      (error "Invalid Google/Youtube URL: %s" url))
    (insert (format "\n<object width=\"%d\" height=\"%d\"" w h)
	    "\ntype=\"application/x-shockwave-flash\"")
    (if googlep
	(setq url (replace-regexp-in-string "/videoplay\\?"
					    "/googleplayer.swf?" url))
      (setq url (replace-regexp-in-string "\\(/watch\\)?\\?v=" "/v/" url)))
    (setq url (replace-regexp-in-string "&.*$" "" url))
    (insert (format "\ndata=\"%s\">" url)
	    "\n<param name=\"movie\""
	    (format "\nvalue=\"%s\" />" url)
	    "\n</object>")
    (indent-region p (point) nil)
    (run-hooks 'lj-youtube-hook)))

(defun lj-insert-journal (name &optional community)
  "Insert a link to NAME journal or LJ community into an LJ post.

Optional prefix argument, COMMUNITY means the link is to a LJ community
instead of a LJ user's journal."
  (interactive "sUser or Community name: \nP")
  (let ((type (if current-prefix-arg "comm" "user"))
	(p (point)))
    (insert (format "\n<lj %s=\"%s\" />\n" type name))
    (indent-region p (point) nil)
    (run-hooks 'lj-journal-hook)))

(defvar lj-abbrev-table nil
  "Abbrev table to use in `lj-mode'.")
(define-abbrev-table 'lj-abbrev-table ())

(define-derived-mode lj-mode html-mode "LJ"
  "This is a mode for composing LiveJournal posts.
Its parent modes are `html-mode' and `sgml-mode' so everything you
need to construct good clean HTML should be right at your fingertips.

LJ specific bindings:

  \\[lj-post]\tSubmit post to LiveJournal
  \\[lj-preview]\t\tPreview post in web browser
  \\[lj-validate]\t\tValidate the markup in the post

  \\[lj-writers-block]\t\tAnswer a LJ \"Writer's Block\" question

  \\[lj-cut-region]\t\tHide text behind a LJ \"cut\"
  \\[lj-insert-journal]\t\tInsert a journal link
  \\[lj-insert-poll]\t\tInsert a poll
  \\[lj-insert-youtube]\t\tInsert a Google or YouTube Video

  \\[lj-goto-subject]\tMove to the Subject header
  \\[lj-goto-fcc]\tMove to the FCC header
  \\[lj-goto-bcc]\tMove to the BCC header
  \\[lj-goto-community]\tMove to the Community header
  \\[lj-goto-music]\tMove to the Music header
  \\[lj-goto-security]\tMove to the Security header
  \\[lj-goto-mood]\tMove to the Mood header
  \\[lj-goto-location]\tMove to the Location header
  \\[lj-goto-userpic]\tMove to the Userpic header
  \\[lj-goto-tags]\tMove to the Tags header
  \\[lj-goto-body]\tMove to the post body

  \\[lj-customise-faces]\t\tSet the header faces
  \\[lj-customise-group]\t\tSet the user options


General bindings:
\\{lj-mode-map}"
  :group 'lj
  :syntax-table nil
  :abbrev-table lj-abbrev-table
  (auto-save-mode 1)
  (abbrev-mode 1))

(add-hook 'lj-mode-hook #'font-lock-mode)

(defun lj-make-archive-name ()
  "Compute a filename for archiving LJ posts.

The filenames are of the format... `ljp-YYYYMMDDHHMM'."
  (let ((file (format-time-string "ljp-%Y%m%d%H%M"))
	(dir lj-archive-directory))
    (expand-file-name file dir)))

(defun lj-generate-new-buffer ()
  "Create a new buffer for writing a new LJ post."
  (or (file-directory-p lj-drafts-directory)
      (make-directory-path lj-drafts-directory))
  (switch-to-buffer
   (find-file-noselect
    (expand-file-name (format-time-string "ljd-%Y%m%d%H%M")
		      lj-drafts-directory)))
  (rename-buffer "*LJ-Post*" 'unique)
  (when (lj-utf-emacs-p)
    (set-buffer-file-coding-system 'utf-8))
  (insert "\n")
  (make-extent (point-min) (point-at-eol))
  (insert "Subject: \n")
  (when lj-archive-posts
    (or (file-directory-p lj-archive-directory)
	(make-directory-path lj-archive-directory))
    (insert (format "FCC: %s\n" (lj-make-archive-name))))
  (when (stringp lj-bcc-address)
    (insert (format "BCC: %s\n" lj-bcc-address)))
  (insert (format "Security: %s\n" lj-default-security-level))
  (when (stringp lj-default-location)
    (insert (format "Location: %s\n" lj-default-location)))
  (when (functionp (symbol-value 'lj-music))
    (insert (format "Music: %s\n" (funcall lj-music))))
  (insert "Mood: \n")
  (when-boundp 'lj-default-pickw
    (insert (format "Userpic: %s\n" lj-default-pickw))
    (lj-update-userpic-glyph (expand-file-name lj-default-pickw
					       lj-userpic-directory)))
  (insert "Tags: \n")
  ;; fool html mode
  (set-extent-property
   (insert-face "</head>\n" 'default) 'invisible t)
  (insert lj-header-separator "\n")
  (when lj-signature
    (save-excursion (insert "\n\n" lj-signature)))
  (lj-mode))

(defun lj-goto-subject (&optional nocreate)
  "Move to the Subject header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Subject: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Subject: ")
	(backward-char 1))))

(defun lj-goto-fcc (&optional nocreate)
  "Move to the FCC header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^FCC: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "FCC: \n")
	(backward-char 1))))

(defun lj-goto-bcc (&optional nocreate)
  "Move to the BCC header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^BCC: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "BCC: \n")
	(backward-char 1))))

(defun lj-goto-security (&optional nocreate)
  "Move to the Security header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Security: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Security: \n")
	(backward-char 1))))

(defun lj-goto-community (&optional nocreate)
  "Move to the Community header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Community: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Community: \n")
	(backward-char 1))))

(defun lj-goto-location (&optional nocreate)
  "Move to the Location header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Location: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Location: \n")
	(backward-char 1))))

(defun lj-goto-mood (&optional nocreate)
  "Move to the Mood header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Mood: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Mood: \n")
	(backward-char 1))))

(defun lj-goto-music (&optional nocreate)
  "Move to the Music header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Music: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Music: \n")
	(backward-char 1))))

(defun lj-goto-userpic (&optional nocreate)
  "Move to the Userpic header of an LJ post buffer.

The header is created if it doesn't exist, unless optional argument
NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Userpic: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Userpic: \n")
	(backward-char 1))))

(defun lj-goto-tags (&optional nocreate)
  "Move to the Tags header of an LJ post buffer.
The header is created if it doesn't exist unless NOCREATE is non-nil."
  (interactive)
  (goto-char (point-min))
  (or (re-search-forward "^Tags: " nil 'missing)
      (unless nocreate
	(goto-char (point-min))
	(insert "Tags: \n")
	(backward-char 1))))

(defun lj-goto-x-lj-itemid (&optional nocreate)
  "Move to the X-LJ-ItemID header."
  (goto-char (point-min))
  (re-search-forward "^X-LJ-ItemID: " nil 'missing))

(defun lj-goto-x-lj-url (&optional nocreate)
  "Move to the X-LJ-URL header."
  (goto-char (point-min))
  (re-search-forward "^X-LJ-URL: " nil 'missing))

(defun lj-goto-body ()
  "Move to the body of an LJ post buffer."
  (interactive)
  (goto-char (point-min))
  (re-search-forward (regexp-quote lj-header-separator) nil t)
  (forward-line 1)
  (goto-char (point-at-bol)))

(defun lj-current-header ()
  "Return the name of the LJ header on the current line, or nil."
  (let ((hregex lj-header-regexp)
	(separator (regexp-quote lj-header-separator)))
    (if (save-excursion (re-search-forward separator nil t))
	(save-restriction
	  (narrow-to-region (point-at-bol) (point-at-eol))
	  (string-match hregex (buffer-string))
	  (substring (buffer-string) (match-beginning 1) (match-end 1)))
      nil)))

(defun lj-header-content (header)
  "Return the content of HEADER as a string."
  (let ((goto (intern-soft (concat "lj-goto-" (downcase header)))))
    (save-excursion
      (funcall goto 'nocreate)
      (buffer-substring-no-properties (point) (point-at-eol)))))

(defun lj-update-userpic-glyph (glyph)
  "Update the userpic, GLYPH, displayed in the LJ-Post buffer."
  (let ((ext (extent-at (point-min)))
	(type (if (featurep '(and sxemacs ffi-magic))
		  (downcase (car (split-string-by-char
				  (declare-fboundp
				   (magic:file-type glyph)) ?\ )))
		(downcase
		 (cadr (split-string-by-char
			(shell-command-to-string (concat "file " glyph))
			?\ ))))))
    (set-extent-begin-glyph
     ext (make-glyph
	  (list (vector (intern-soft type)
			:data (with-temp-buffer
				(insert-file-contents-literally glyph)
				(buffer-string))))))))

;; Header completion
(defvar lj-completion-time 3
  "Time in seconds before completion list is reset.")

(defvar lj-completion-timer (make-itimer)
  "Completion timer.")

(defvar lj-completion-list nil
  "Completion list.")

(defvar lj-header-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?~  "w " table)
    (modify-syntax-entry ?`  "w " table)
    (modify-syntax-entry ?-  "w " table)
    (modify-syntax-entry ?_  "w " table)
    (modify-syntax-entry ?+  "w " table)
    (modify-syntax-entry ?{  "w " table)
    (modify-syntax-entry ?[  "w " table)
    (modify-syntax-entry ?}  "w " table)
    (modify-syntax-entry ?]  "w " table)
    (modify-syntax-entry ?\\ "w " table)
    (modify-syntax-entry ?|  "w " table)
    (modify-syntax-entry ?\; "w " table)
    (modify-syntax-entry ?'  "w " table)
    (modify-syntax-entry ?<  "w " table)
    (modify-syntax-entry ?>  "w " table)
    (modify-syntax-entry ?#  "w " table)
    (modify-syntax-entry ?\  "w " table)
    (modify-syntax-entry ?.  "w " table)
    table)
  "Syntax table used in funky header cycling completion.")

(defun lj-init-completion-timer ()
  "Initialise the completion timer."
  (let ((timer lj-completion-timer))
    (set-itimer-function timer #'(lambda ()
				   (setq lj-completion-list nil)))
    (set-itimer-value timer lj-completion-time)))
(add-hook 'lj-init-hook #'lj-init-completion-timer)

(defsubst lj-cycle-list (list &optional reverse)
  "Return a list of head of LIST, and LIST rotated 1 place forward.

If optional argument, REVERSE is non-nil, rotate the list in the other
direction."
  (if (featurep 'sxemacs)
      (let ((list (apply #'dllist list))
	    name)
	(if reverse
	    (dllist-rrotate list)
	  (dllist-lrotate list))
	(setq name (dllist-car list))
	(list name (dllist-to-list list)))
    ;; XEmacs
    (if reverse
	(let* ((name (car (last list)))
	       (l1 (cdr (reverse list)))
	       (l2 (reverse l1)))
	  (push name l2)
	  (list name l2))
      (let* ((name (cadr list))
	     (oldcar (car list))
	     (list (cdr list))
	     (list (append list (list oldcar))))
	(list name list)))))

(defsubst lj-set-completion-timer ()
  "(Re)set completion timer's value."
  (let ((timer lj-completion-timer))
    (and (itimerp timer)
	 (set-itimer-value timer lj-completion-time))))

(defun lj-complete-header-backwards ()
  "Complete header, cycling backwards."
  (interactive)
  (and (lj-current-header)
       (lj-complete-header 'reverse)
       (when (string-match (lj-current-header) "Userpic")
	 (when (file-exists-p (expand-file-name
			       (lj-header-content "userpic")
			       lj-userpic-directory))
	   (lj-update-userpic-glyph
	    (expand-file-name (lj-header-content "userpic")
			      lj-userpic-directory))))))

(defun lj-complete-header (&optional reverse)
  "Completion for LJ headers.

This completion does not pop up any completion buffers, instead it
cycles through the possible completions \"in-place\" with each
successive TAB.

With non-nil optional argument, REVERSE, the cycling goes in the other
direction."
  (interactive)
  (unless lj-completion-list
    (unless (itimer-live-p lj-completion-timer)
      (lj-set-completion-timer)
      (activate-itimer lj-completion-timer))
    (let* ((completion-ignore-case t)
	   (type (lj-current-header))
	   (table (cond ((string= type "Security")
			 (let ((groups (copy-sequence
					(append lj-groups
						'(("public" . ?a)
						  ("private" . ?b)
						  ("usemask" . ?c))))))
			   (sort* groups #'string-lessp :key #'car)))
			((string= type "Mood")
			 (let ((moods (copy-sequence lj-moods)))
			   (sort* moods #'string-lessp :key #'car)))
			((string= type "Userpic")
			 (let ((pics (copy-sequence lj-pickws)))
			   (sort* pics #'string-lessp :key #'car)))
			((string= type "Tags")
			 (let ((tags (mapcar #'(lambda (e) (cons e ?a))
					     lj-tags)))
			   (sort* tags #'string-lessp :key #'car)))
			(t (error 'invalid-argument type))))
	   (current (if (string-match (current-word) type)
			""
		      (current-word)))
	   (completion (try-completion current table))
	   (all (all-completions current table)))
      (if (null completion)
	  (message "Can't find completion for \"%s\"" current)
	(setq lj-completion-list all))))
  (when lj-completion-list
    (multiple-value-bind (completion newlist)
	(lj-cycle-list lj-completion-list reverse)
      (setq lj-completion-list newlist)
      (with-syntax-table lj-header-syntax-table
	(unless (string= "" (current-word))
	  (unless (eolp)
	    (forward-word))
	  (unless (string-match (lj-current-header) (current-word))
	    (backward-delete-word)))
	(insert " " completion)))
    (lj-set-completion-timer)))

(defun lj-sgml-indent-tab-or-complete (&optional refresh)
  "Does completion if in LJ headers, `sgml-indent-or-tab' otherwise.

If point is after the header separator, this function simply calls
`sgml-indent-or-tab'.  If point is in the headers section it will do
completion relevent to the header on the current line.

Please note that this is \"inline\" completion, that means you won't
be prompted for anything in the minibuffer.  The completions will
cycle directly in the LJ-post buffer.

The different header completions are:

   Subject: Sweet bugger all.  Sorry, haven't perfected read-mind-mode
            yet.

       FCC: Computes a new archive filename.

       BCC: BBDB email addresses

  Security: Completes valid security levels.  With prefix arg REFRESH,
            update your list of friends groups from livejournal.com

 Community: No completion, just insert a TAB.

     Music: Refreshes to the currently current song

      Mood: Completes moods.  With prefix arg REFRESH, update the list
            of moods from livejournal.com.

  Location: No completion.

   Userpic: Completes list of LJ userpic keywords you have defined.
            With prefix arg REFRESH, update you list of userpic
            keywords.

      Tags: Multiple completion from your list of previously used tags.
            With prefix arg REFRESH, update your list of tags from
            livejournal.com."
  (interactive "P")
  (let ((header (lj-current-header)))
    (if header
	(cond ((string= header "Subject")
	       (error "Sorry, me crystal ball is in for repairs"))
	      ((string= header "FCC")
	       (let ((new (lj-make-archive-name)))
		 (goto-char (point-at-bol))
		 (re-search-forward "^FCC: " (point-at-eol))
		 (delete-region (point) (point-at-eol))
		 (insert new)))
	      ((string= header "BCC")
	       (if-fboundp #'bbdb-complete-name
		   (progn
		     (goto-char (point-at-eol))
		     (bbdb-complete-name))
		 (expand-abbrev)))
	      ((string= header "Security")
	       (when (or refresh (not lj-groups))
		 (lj-get-friends-groups))
		 (goto-char (point-at-bol))
		 (re-search-forward "^Security: " (point-at-eol))
		 (lj-complete-header))
	      ((string= header "Community")
	       (goto-char (point-at-eol))
	       (insert "\t"))
	      ((string= header "Music")
	       (let ((current (and (functionp (symbol-value 'lj-music))
				   (funcall lj-music))))
		 (when current
		   (goto-char (point-at-bol))
		   (re-search-forward "^Music: " (point-at-eol))
		   (delete-region (point) (point-at-eol))
		   (insert current))))
	      ((string= header "Mood")
	       (when (or refresh (not lj-moods))
		 (lj-get-moods))
	       (goto-char (point-at-bol))
		 (re-search-forward "^Mood: " (point-at-eol))
		 (lj-complete-header))
	      ((string= header "Location")
	       (error "If you don't know, I can't help you"))
	      ((string= header "Userpic")
	       (when (or refresh (not lj-pickws))
		 (lj-get-pickws))
		 (goto-char (point-at-bol))
		 (re-search-forward "^Userpic: " (point-at-eol))
		 (lj-complete-header)
		 (when (file-exists-p (expand-file-name
				       (lj-header-content "userpic")
				       lj-userpic-directory))
		   (lj-update-userpic-glyph
		    (expand-file-name (lj-header-content "userpic")
				      lj-userpic-directory))))
	      ((string= header "Tags")
	       (when (or refresh (not lj-tags))
		 (lj-get-tags))
	       (lj-complete-header))
	      (t
	       (error "Unknown LJ header: %s" header)))
      (sgml-indent-or-tab))))

(defregexp lj-url-regexp
  (concat "\\(https?://\\|s?ftp://\\|gopher://\\|telnet://"
	  "\\|wais://\\|file:/\\|s?news:\\)"
	  "[^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;\\(&gt\\)]+")
  "A regular expression matching URL's.")

(defregexp lj-email-regexp
  "[-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+"
  "A regular expression matching email addresses.")

(defun lj-text-to-html (beg end &optional nopbr)
  "Convert the plain text in the region BEG - END to html.

With optional argument, NOPBR, don't add <p>..</p> or <br /> tags.

This is an extremely basic converter.  All it really does is wrap
paragraphs in <p>...</p>, and add <br /> to the end of each non-blank
line.  It will also convert old 70's style text highlighting to the
HTML equivalent.  e.g. _text_ -> <u>text</u>, *text* -> <b>text</b>.
It also converts non-ASCII to HTML entities, and converts URL's and
email addresses to hyperlinks.  Email addresses are obfuscated in an
attempt to protect against spam harvesters.

Apart from the bold, underline, and hyperlink stuff, that's all the
eye-candy you'll get.  Forget fonts, colours, tables, and lists.
That's not what this is about.  The idea is to keep the text as close
to \"as-is\" without resorting to using <pre>...</pre> tags.

Calling this function on text that contains \"<lj*>\" will break those
tags.  So take note of what you are doing."
  (let ((replacements '(("&" . "&amp;")
			("\\.\\.\\." . "&hellip;")
			("<" . "&lt;")
			(">" . "&gt;")
			("\"" . "&quot;")
			("_\\(.*\\)_" . "<u>\\1</u>")
			("\\*\\(.*\\)\\*" . "<b>\\1</b>")))
	(url lj-url-regexp)
	(email lj-email-regexp))
    (unless nopbr
      (add-to-list 'replacements
		   (cons (if (featurep 'sxemacs)
			     "\\([[:alnum:][:punct:]]\\)\n"
			   "\\([a-zA-Z0-9]\\|\\s.\\)\n")
			 "\\1<br />\n") 'append))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; html quoting
      (mapcar
       #'(lambda (rep)
	   (save-excursion
	     (while (re-search-forward (car rep) nil t)
	       (replace-match (cdr rep) t))))
       replacements)
      ;; paragraphs
      (unless nopbr
	(save-excursion
	  (while (not (eobp))
	    (save-restriction
	      (mark-paragraph)
	      (narrow-to-region (point) (mark))
	      (goto-char (point-min))
	      (insert "<p>")
	      (goto-char (point-max))
	      (insert "</p>\n"))
	    (forward-paragraph))))
      ;; urls
      (save-excursion
	(while (re-search-forward url nil t)
	  (replace-match "<a href=\"\\&\">\\&</a>")))
      ;; emails
      (save-excursion
	(while (re-search-forward email nil t)
	  (replace-match "<a href=\"mailto:\\&\">\\&</a>"))
	(while (search-backward "@" nil t)
	  (replace-match "&#64;" nil t)))
      ;; entities
      (save-excursion
	(lj-entify-region (point-min) (point-max))))
    (when (region-exists-p)
      (zmacs-deactivate-region))))

(defun lj-ljtags-to-html ()
  "Convert \"<lj-*>\" tags to something resembling HTML.

This function is used so that the markup in a post can be validated
before it is submitted, and also so the post can be previewed before
it is submitted.  Do not expect anything fancy."
  (goto-char (point-min))
  ;; polls
  (save-excursion
    (while (re-search-forward "<lj-\\(poll\\)" nil t)
      (let ((p (point-at-bol)))
	(search-forward (concat "</lj-" (match-string 1) ">") nil t)
	(save-restriction
	  (narrow-to-region p (point))
	  (lj-text-to-html (point-min) (point-max) 'nopbr)
 	  (goto-char (point-min))
 	  (insert "<div class=\"ljpoll\">\n<pre>\n")
 	  (goto-char (point-max))
 	  (insert "\n</pre>\n</div>")))))
  ;; cuts
  (save-excursion
    (while (re-search-forward "^</?lj-cut\\( text=\"---More---\"\\)?>$" nil t)
      (lj-text-to-html (match-beginning 0) (match-end 0) 'nopbr)))
  (save-excursion
    (while (re-search-forward "&lt;lj-cut" nil t)
      (replace-match "<div class=\"ljcut\">\n\\&")
      (re-search-forward "&lt;/lj-cut&gt;" nil t)
      (replace-match "\\&\n</div>")))
  ;; journal links
  (save-excursion
    (while (re-search-forward "<lj user=\"\\(.*\\)\" />" nil t)
      (replace-match "<a href=\"http://\\1.livejournal.com/profile\">
  <img src=\"http://p-stat.livejournal.com/img/userinfo.gif\"
    alt=\"[info]\" width=\"17\" height=\"17\"
    style=\"vertical-align: bottom; border: 0; padding-right: 1px;\" />
</a>
<a href=\"http://\\1.livejournal.com/\"><b>\\1</b></a>")))
  ;; writer's block
  (save-excursion
    (while (re-search-forward "<lj-template name=\"qotd\" id=\"[0-9]+\" />"
			      nil t)
      (replace-match "<h3>Writer's Block Answer</h3>" t))))

(defconst lj-byline
  (format (concat "<br />"
		  "<p style=\"text-align:right;font-family:verdana,"
		  "helvetica,sans-serif;font-size:8pt;\">"
		  "Posted via: <a href=\"http://www.sxemacs.org/lj.el\""
		  " title=\"Download SXEmacs/LJ\">"
		  "SXEmacs/LJ</a> (%s)</p>")
	  lj-clientversion)
  "A client by-line to add to bottom of a post.

It is also included when validating or previewing a post.")

(defun lj-validate ()
  "Check the markup in a LJ post.

Please note that livejournal.com is quite forgiving when it comes to
HTML in journal entries, lj.el, on the other hand... isn't.  For
your entry to pass this validation it needs to be valid XHTML 1.0
Transitional."
  (interactive)
  (run-hooks 'lj-before-validate-hook)
  (let ((vf (make-temp-name (expand-file-name "LJ-" (temp-directory))))
	(pb (current-buffer)))
    (with-current-buffer (get-buffer-create vf)
      (erase-buffer)
      (insert lj-validate-header)
      (insert
       (save-excursion
	 (set-buffer pb)
	 (lj-goto-body)
	 (save-restriction
	   (narrow-to-region (point) (point-max))
	   (buffer-string))))
      (and lj-self-promote
	   (insert lj-byline))
      (insert lj-validate-footer)
      (lj-ljtags-to-html)
      (write-region (point-min) (point-max) vf))
    (unwind-protect
	(progn
	  (find-file vf)
	  (sgml-parse-prolog)
	  (sleep-for 5)
	  (sgml-validate (sgml-default-validate-command))
	  (let ((proc (get-buffer-process "*sgml validation*")))
	    (while (process-live-p proc)
	      (sit-for 0.1)
	      (message "Validating markup, please wait..."))
	    (message "Validation complete!")
	    (when (> (process-exit-status proc) 0)
	      (error 'syntax-error (process-name proc)))))
      (kill-buffer vf)
      (delete-file vf)
      (switch-to-buffer pb))
    (run-hooks 'lj-after-validate-hook)))

(defun lj-preview-headers (buf)
  "Add htmlised LJ headers in buffer, BUF for `lj-preview'."
  (let (text pic)
    (save-excursion
      (save-restriction
	(set-buffer buf)
	(setq pic (lj-header-content "userpic"))
	(lj-goto-body)
	(narrow-to-region (point-min) (point))
	(setq text (buffer-substring-no-properties)))
      (widen)
      (with-temp-buffer
	(insert text)
	(lj-text-to-html (point-min) (point-max))
	(goto-char (point-min))
	(insert "<div class=\"ljhead\">\n")
	(and (search-forward "<p>")
	     (insert (format "<img src=\"%s\" align=\"right\" alt=\"Userpic\" />"
			     (cdr (assoc pic lj-pickws)))))
	(while (search-forward "&lt;/head&gt;" nil t)
	  (replace-match "" nil t))
	(goto-char (point-max))
	(insert "\n</div>\n\n")
	(buffer-string)))))

(defun lj-preview ()
  "Preview the LJ post in a web browser.

Please note that this is far from a true representation of what the
thing will look like once it has been submitted to LiveJournal.  But
it should give you a rough idea."
  (interactive)
  (run-hooks 'lj-before-preview-hook)
  (let ((vf (make-temp-name (expand-file-name "LJ-" (temp-directory))))
	(pb (current-buffer)))
    (with-current-buffer (get-buffer-create vf)
      (erase-buffer)
      (insert lj-validate-header)
      (insert (lj-preview-headers pb))
      (insert "<div class=\"lj\">\n")
      (insert
       (save-excursion
	 (set-buffer pb)
	 (lj-goto-body)
	 (save-restriction
	   (narrow-to-region (point) (point-max))
	   (buffer-string))))
      (and lj-self-promote
	   (insert lj-byline))
      (insert "\n</div>")
      (insert lj-validate-footer)
      (lj-ljtags-to-html)
      (browse-url-of-buffer))
    (when (region-exists-p)
      (zmacs-deactivate-region))
    (run-hooks 'lj-after-preview-hook)))

(defvar lj-last-url "No URL yet, got nothing to blog about?"
  "The URL to your last posted blog entry on LiveJournal.")

(defvar lj-item-id ""
  "The itemid of the last post.")

(defun lj-post-proc-parser (buf)
  "Process parser for `lj-post'.
Argument BUF is the process buffer used."
  (let ((url "^url\n\\(.*$\\)")
	(itemid "^itemid\n\\(.*$\\)"))
    (with-current-buffer buf
      (when (lj-proc-success)
	(setq lj-last-user-set-time nil)
	(goto-char (point-min))
	(save-excursion
	  (if (re-search-forward url nil t)
	      (setq lj-last-url (match-string 1))
	    (setq lj-last-url "NO URL RETURNED FROM LiveJournal")))
	(save-excursion
	  (and (re-search-forward itemid nil t)
	       (setq lj-item-id (match-string 1))))
	(kill-buffer nil)))))

(defun lj-archive-post (archive)
  "Archive the current post to ARCHIVE."
  (let ((buf (current-buffer)))
    (with-current-buffer (find-file-noselect archive)
      (when (zerop (length (lj-header-content "x-lj-itemid")))
	(insert (format "\nX-LJ-URL: %s\n" lj-last-url)
		(format "X-LJ-ItemID: %s" lj-item-id))
	(insert-buffer buf))
      (save-buffer)
      (kill-buffer nil))))

(defun lj-send-bcc (subject security tags comm mood music location body)
  "Send a copy of a LJ post via email to `lj-bcc-address'.

Argument SUBJECT is the subject header from the post.

Argument SECURITY is the security level from the post, it is added to
them mail as X-LJ-Auth header.

Argument TAGS are the tags from the post, added as Keywords header.

Argument COMM is the community from the post, added as X-LJ-Community
header

Argument MOOD is the mood from the post, added as X-LJ-Mood header.

Argument MUSIC is the music from the post, added as X-Now-Playing
header.

Argument LOCATION is the location from the post, added as X-LJ-Location
header

Argument BODY is of course the post's body."
  (let* ((from (concat user-full-name
		       " <" lj-user-id "@livejournal.com>"))
	 (headers `(("From" . ,from)
		    ("Keywords" . ,tags)
		    ("X-LJ-Auth" . ,security)
		    ("X-LJ-Community" . ,comm)
		    ("X-LJ-Location" . ,location)
		    ("X-LJ-Mood" . ,mood)
		    ("X-Now-Playing" . ,music)
		    ("X-URL" . ,lj-last-url)
		    ("MIME-Version" . "1.0")
		    ("Content-Type" . "text/html")))
	 (mail-user-agent 'sendmail-user-agent))
    (compose-mail lj-bcc-address subject headers)
    (goto-char (point-max))
    (insert body)
    (declare-fboundp (mail-send-and-exit nil))))

(defun lj-last-entry-proc-parser (buf)
  "Process the output from `lj-get-last-entry-btime'.
Argument BUF is the process buffer used."
  (let ((regexp "^events_1_eventtime\n\\(.*$\\)"))
    (with-current-buffer buf
      (when (lj-proc-success)
	(goto-char (point-min))
	(re-search-forward regexp nil t)
	(setq lj-last-entry-btime
	      (apply #'encode-btime (lj-parse-time-string
				     (match-string 1))))
	(kill-buffer nil)))))

(defun lj-get-last-entry-btime ()
  "Leech the last entry from LJ to get it's date/time."
  (let ((cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(url (concat lj-base-url
		     "?mode=getevents"
		     "&user=" lj-user-id
		     "&auth_method=cookie"
		     (format "&ver=%d" (if (lj-utf-emacs-p) 1 0))
		     "&noprops=1"
		     "&selecttype=lastn"
		     "&howmany=1")))
    (lj-http-post url cookies #'lj-last-entry-proc-parser)))

(defun lj-delete-post-proc-parser (buf)
  "Process the output from `lj-delete-post'.

Argument BUF is the process buffer that was used."
  (with-current-buffer buf
    (when (lj-proc-success)
      (message "Your post has been successfully removed from LiveJournal.")
      (kill-buffer nil))))

(defun lj-stringify-id (id)
  "Returns a string version of the number, ID."
  (if (stringp id)
      id
    (and (numberp id)
	 (number-to-string id))))

(defun lj-delete-post-internal (itemid)
  "Delete the post with ITEMID."
  (let ((cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(url (concat lj-base-url
		     "?mode=editevent"
		     "&user=" lj-user-id
		     "&auth_method=cookie"
		     (format "&ver=%d" (if (lj-utf-emacs-p) 1 0))
		     "&itemid=" (lj-stringify-id itemid)
		     "&prop_useragent=" (lj-hexify-string lj-useragent)
		     "&event=")))
    (lj-http-post url cookies #'lj-delete-post-proc-parser)))

(defun lj-set-date/time ()
  "Return an internal time value to use as post date/time.

This will prompt for a date string of the format yyyy-mm-dd, and a
time string in the format HH:MM \(24hr\).  If either are given a null
string the current date/time are used.

The value returned is that same as from `encode-time'."
  (let* ((date (read-string (format-time-string "New date [%Y-%m-%d]: ")
			    nil nil
			    (format-time-string "%Y-%m-%d")))
	 (time (read-string (format-time-string "New time [%H:%M]: ")
			    nil nil
			    (format-time-string "%H:%M")))
	 (timestr (concat date " " time))
	 (btime (apply #'encode-btime (lj-parse-time-string timestr))))
    (btime-to-time btime)))

(defun lj-twitter-compress-url (url)
  "Compress URL using tinyurl.com."
  (with-temp-buffer
    (mm-url-insert
     (concat "http://tinyurl.com/api-create.php?url="
	     (lj-hexify-string url t)))
    (buffer-string)))

(defun lj-twitter-sentinel (process status)
  "Sentinel for `lj-twitter-update-status' PROCESS STATUS."
  (if (equal status "finished\n")
      (message "Sending to Twitter...done")
    (message "Sending to Twitter...failed: %s"
	     (substring status 0 (1- (length status))))))

;;; FIXME: Twitter no longer supports Basic Auth, so must rewrite this
;;; using OAuth.
(defun lj-twitter-update-status (user pass status url)
  "Update twitter status.

Argument USER is your twitter username.
Argument PASS is your twitter password.
Argument STATUS is the subject header from your LJ post.
Argument URL is the URL to the post on livejournal.com."
  (let* ((userpass (format "%s:%s" user pass))
	 (turl (lj-twitter-compress-url url))
	 (twit (concat "status="
		       (lj-hexify-string
			(concat status "  See: " turl) t)))
	 (twiturl "http://twitter.com/statuses/update.json")
	 proc)
    (if (<= (length twit) 147)		; twitter's max + "status="
	(progn
	  (setq proc
		(apply #'start-process
		       "LJcurl" nil "curl"
		       (list "-u" userpass
			     "-d" twit
			     "-s" twiturl
			     "-H" "X-Twitter-Client: SXEmacs_LJ"
			     "-H" (format "X-Twitter-Client-Version: %s"
					  lj-version)
			     "-H" (concat
				   "X-Twitter-Client-URL: "
				   "http://www.sxemacs.org/~steve/lj/lj.xml")
			     "-d" "source=lj.el")))
	  (set-process-sentinel proc #'lj-twitter-sentinel))
      (warn "LJ subject too long for Twitter"))))

(defun lj-check-limits (bodlen sublen taglen muslen loclen)
  "Make sure we don't exceed any LJ size limits.

Argument BODLEN: length of the body of a post.  Max is 65535 bytes.
Argument SUBLEN: length of the subject header.  Max is 255 chars.
Argument TAGLEN: length of the tags header.  Max is 100 chars.
Argument MUSLEN: length of the music header.  Max is 100 chars.
Argument LOCLEN: length of the location header.  Max is 100 chars."
  (let ((maxbod 65535)
	(maxsub 255)
	(maxtag 100)
	(maxmus 100)
	(maxloc 100))
    (and (> bodlen maxbod)
	 (error 'invalid-argument
		(format "Post body exceeds maximum by %d chars"
			(- bodlen maxbod))))
    (and (> sublen maxsub)
	 (error 'invalid-argument
		(format "Subject exceeds maximum by %d chars"
			(- sublen maxsub))))
    (and (> taglen maxtag)
	 (error 'invalid-argument
		(format "Tags exceed maximum by %d chars"
			(- taglen maxtag))))
    (and (> muslen maxmus)
	 (error 'invalid-argument
		(format "Music header exceeds maximum by %d chars"
			(- muslen maxmus))))
    (and (> loclen maxloc)
	 (error 'invalid-argument
		(format "Location header exceeds maximum by %d chars"
			(- loclen maxloc))))))

(defun lj-post (&optional out-of-order)
  "Submit a new post to LiveJournal.

With a single prefix argument, OUT-OF-ORDER, prompt for a date/time to
use for the post.

With two prefix args, also set a \"date out of order\" flag."
  (interactive "i")
  (run-hooks 'lj-before-post-hook)
  (let ((subject (lj-header-content "subject"))
	(body (and (lj-goto-body)
		   (if lj-self-promote
		       (concat 
			(buffer-substring-no-properties (point) (point-max))
			lj-byline)
		     (buffer-substring-no-properties (point) (point-max)))))
	(user lj-user-id)
	(security (lj-header-content "security"))
	(tags (lj-header-content "tags"))
	(comm (lj-header-content "community"))
	(mood (lj-header-content "mood"))
	(location (lj-header-content "location"))
	(music (lj-header-content "music"))
	(pickw (lj-header-content "userpic"))
	(itemid (lj-header-content "x-lj-itemid"))
	(cookies (or lj-cookies
		     (error "No LJ cookies found")))
	(backdated nil)
	(date nil)
	(draftid (buffer-file-name))
	url)
    (when (and out-of-order
	       (null current-prefix-arg))
      (setq backdated t
	    date lj-last-user-set-time))
    (cond ((eq (car current-prefix-arg) 4)
	   (setq date (lj-set-date/time)))
	  ((eq (car current-prefix-arg) 16)
	   (setq date (lj-set-date/time)
		 backdated t)))
    (lj-check-limits (length body)
		     (length subject)
		     (length tags)
		     (length music)
		     (length location))
    (setq url (lj-construct-url subject body user security tags comm nil nil
				mood location music pickw date backdated
				itemid))
    ;; lets save the draft out to disc just in case something goes wrong
    (save-buffer)
    (lj-http-post url cookies #'lj-post-proc-parser)
    (and lj-archive-posts
	 (lj-archive-post (lj-header-content "fcc")))
    (and lj-bcc-address
	 (lj-send-bcc subject security tags comm mood music location body))
;;; FIXME: twitter updates are broken, needs OAuth
;;    (and lj-twitter-flag
;;	 (lj-twitter-update-status lj-twitter-username lj-twitter-password
;;				   subject lj-last-url))
    ;; If there is a itemid don't delete the draft because it is our
    ;; archive copy
    (when (zerop (length itemid))
      (delete-file draftid))
    (run-hooks 'lj-after-post-hook)))

;; keep track of the date of the last entry for backdating purposes
(add-hook 'lj-after-post-hook #'lj-get-last-entry-btime)

;;; Writer's Block
(defvar lj-qotd-buffer "*LJ Writer's Block*"
  "Buffer displaying a list of LJ Writer's Block questions.")

(defun lj-parse-qotd-archive ()
  "Leech the qotd archive and make it presentable for human consumption."
  (let ((buf (get-buffer-create lj-qotd-buffer))
	(bregexp "<!-- Content -->")
	(eregexp "<p class='skiplinks'>")
	(qregexp "<p class='qotd-archive-item-question'>\\(.*\\)</p><p")
	(dregexp
	 (concat "<div class=\"b-qotd-question\">"
		 "<p class='qotd-archive-item-date'>\\(.*[0-9]+\\)</p>"))
	(idregexp "^.*qotd=\\([0-9]+\\).*\n.*$")
	(url "http://www.livejournal.com/misc/qotdarchive.bml")
	b e)
    (with-current-buffer buf
      (when (lj-utf-emacs-p)
	(set-buffer-file-coding-system 'utf-8))
      (erase-buffer)
      (mm-url-insert url)
      (goto-char (point-min))
      (setq b (and (search-forward bregexp nil t)
		   (forward-line 3)
		   (point-at-bol))
	    e (and (search-forward eregexp nil t)
		   (point-at-bol)))
      (narrow-to-region b e)
      (goto-char (point-min))
      (insert (make-string 72 ?=) "\n")
      (save-excursion
	(while (re-search-forward dregexp nil t)
	  (replace-match (format "%s:\n\n" (match-string 1)) t)))
      (save-excursion
	(while (re-search-forward qregexp nil t)
	  (replace-match (format "QOTD: %s\n" (match-string 1)) t)))
      (save-excursion
	(while (re-search-forward "^QOTD:" nil t)
	  (fill-paragraph nil)))
      (save-excursion
	(while (re-search-forward idregexp nil t)
	  (replace-match (concat (format "\nWriter's Block ID: %s\n"
					 (match-string 1))
				 (make-string 72 ?=)) t))))))

(defun lj-qotd-subject (qotd)
  "Update the Subject header with title of QOTD."
  (let ((url (format "http://www.livejournal.com/update.bml?qotd=%d"
		     qotd))
	(subject))
    (with-temp-buffer
      (mm-url-insert url)
      (goto-char (point-min))
      (when (re-search-forward "Writer&#39;s Block: \\(.*\\)\" name="
			       nil t)
	(setq subject (match-string 1))))
    (lj-goto-subject)
    (goto-char (point-at-eol))
    (insert subject)
    (lj-goto-body)
    (forward-line 2)))

(defun lj-narrow-to-qotd (qotd)
  "Narrow Writer's Block buffer to a single QOTD."
  (let ((delim (make-string 72 ?=))
	b e)
    (goto-char (point-max))
    (setq e (and (search-backward (format "ID: %d" qotd))
		 (point-at-eol))
	  b (search-backward delim))
    (narrow-to-region b e)
    (recenter)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (unless (eq major-mode 'lj-mode)
      (switch-to-buffer "*LJ-Post*"))
    (unless (zerop lj-qotd)
      (and (lj-goto-body)
	   (insert (format "<lj-template name=\"qotd\" id=\"%d\" />\n\n"
			   lj-qotd)))
      (lj-qotd-subject lj-qotd))))

(defun lj-cleanup-qotd ()
  "Reset `lj-qotd' to zero and kill the qotd buffer."
  (progn
    (setq lj-qotd 0)
    (delete-other-windows)
    (when (buffer-live-p (get-buffer lj-qotd-buffer))
      (kill-buffer lj-qotd-buffer))))

(defun lj-qotd-quit ()
  "Cancel a LJ \"Writer's Block\" composition."
  (interactive)
  (other-window 1)
  (unless (eq major-mode 'lj-mode)
    (switch-to-buffer "*LJ-Post*"))
  (lj-cleanup-qotd)
  (kill-region (lj-goto-subject) (point-at-eol))
  (kill-region (lj-goto-tags) (point-at-eol))
  (lj-goto-body)
  (remove-hook 'lj-after-post-hook #'lj-cleanup-qotd))

(defun lj-writers-block ()
  "Compose an answer to a LJ \"Writer's Block\" question."
  (interactive)
  (and (kill-region (lj-goto-subject) (point-at-eol))
       (insert "Writer's Block: "))
  (and (kill-region (lj-goto-tags) (point-at-eol))
       (insert "writer's block"))
  (let ((b (lj-goto-body))
	(e (or (and (goto-char (point-max))
		    (search-backward lj-signature nil t))
	       (point-max))))
    (kill-region b e)
    (insert "\n\n"))
  (lj-sgml-indent-tab-or-complete)
  (lj-parse-qotd-archive)
  (pop-to-buffer lj-qotd-buffer)
  (local-set-key [space] #'scroll-up)
  (local-set-key [delete] #'scroll-down)
  (local-set-key [return]
		 #'(lambda ()
		     (interactive)
		     (setq lj-qotd (read-number "Select Writer's Block ID: " t))
		     (lj-narrow-to-qotd lj-qotd)))
  (local-set-key [q] #'lj-qotd-quit)
  (message "[SPC]/[DEL] to scroll, [q] to cancel, [RET] to enter QOTD ID")
  (add-one-shot-hook 'lj-after-post-hook #'lj-cleanup-qotd 'append))

(defun lj-session-auto-save-files ()
  "Return a list of auto-save files in `lj-drafts-directory'."
  (directory-files lj-drafts-directory nil
		   #'auto-save-file-name-p 'list t))

(defun lj-recover-drafts (files)
  "Recover auto-saved FILES in `lj-drafts-directory'."
  (let ((default-directory lj-drafts-directory))
    (while files
      (recover-file (auto-save-original-name (car files)))
      (lj-edit-draft (auto-save-original-name (car files)))
      (setq files (cdr files)))))

;;; Globals

(defun lj ()
  "Compose a new LiveJournal entry."
  (interactive)
  (run-hooks 'lj-init-hook)
  ;; Maybe update tags, groups, moods, pic keywords
  (or lj-tags (lj-get-tags))
  (or lj-groups (lj-get-friends-groups))
  (or lj-moods (lj-get-moods))
  (or lj-default-pickw (lj-get-pickws))
  (let ((auto-saves (lj-session-auto-save-files)))
    (if (and auto-saves
	     (y-or-n-p "Auto saved drafts exist, do you wish to recover "))
	(lj-recover-drafts auto-saves)
      (lj-generate-new-buffer))))

(defun lj-blog-buffer (buffer &optional noformat)
  "Use contents of BUFFER to compose LJ entry.

With optional prefix arg, NOFORMAT, don't attempt to convert the text
to HTML."
  (interactive "bBuffer to blog: \nP")
  (let ((blog (with-temp-buffer
		(insert-buffer buffer)
		(unless current-prefix-arg
		  (lj-text-to-html (point-min) (point-max)))
		(buffer-substring-no-properties))))
    (lj)
    (insert blog)))

(defun lj-blog-region (beg end &optional noformat)
  "Compose LJ entry using content of region BEG - END.

With optional prefix arg, NOFORMAT, dont' attempt to convert the text
to HTML."
  (interactive "r\nP")
  (let ((blog (buffer-substring beg end)))
    (unless current-prefix-arg
      (with-temp-buffer
	(insert blog)
	(lj-text-to-html (point-min) (point-max))
	(setq blog (buffer-substring-no-properties))))
    (lj)
    (insert blog)))

(defun lj-edit-draft (draft)
  "Edit an existing draft previously saved from lj.el."
  (interactive (list
		(read-file-name "Edit draft: "
				lj-drafts-directory "" t)))
  (if (or (zerop (length draft))
	  (not (file-readable-p (expand-file-name draft))))
      (error 'invalid-argument draft)
    (switch-to-buffer (find-file-noselect (expand-file-name draft)))
    (rename-buffer "*LJ-draft*" 'unique)
    (goto-char (point-min))
    (make-extent (point) (point-at-eol))
    (lj-update-userpic-glyph
     (expand-file-name (lj-header-content "userpic")
		       lj-userpic-directory))
    (re-search-forward lj-header-separator nil t)
    (forward-line -1)
    (set-extent-property
     (make-extent (point-at-bol) (1+ (point-at-eol))) 'invisible t)
    (lj-goto-body)
    (lj-mode)))

(defun lj-edit-old-post (post)
  "Edit an already posted LJ entry."
  (interactive (list
		(read-file-name "Edit Post: "
				lj-archive-directory "" t)))
  (if (or (zerop (length post))
	  (not (file-readable-p (expand-file-name post))))
      (error 'invalid-argument post)
    (switch-to-buffer (find-file-noselect (expand-file-name post)))
    (rename-buffer "*LJ-EDIT*" 'unique)
    (and (zerop (length (lj-header-content "x-lj-itemid")))
	 (error "ItemID missing, CANNOT edit this post from SXEmacs/LJ"))
    (goto-char (point-min))
    (make-extent (point) (point-at-eol))
    (lj-update-userpic-glyph
     (expand-file-name (lj-header-content "userpic")
		       lj-userpic-directory))
    (re-search-forward lj-header-separator nil t)
    (forward-line -1)
    (set-extent-property
     (make-extent (point-at-bol) (1+ (point-at-eol))) 'invisible t)
    (lj-goto-body)
    (lj-mode)))

(defun lj-delete-old-post (post)
  "Delete a post from Livejournal."
  (interactive (list
		(read-file-name "Delete Old Post: "
				lj-archive-directory "" t)))
  (if (or (zerop (length post))
	  (not (file-readable-p (expand-file-name post))))
      (error 'invalid-argument post)
    (switch-to-buffer (find-file-noselect (expand-file-name post)))
    (rename-buffer "*LJ-EDIT*" 'unique)
    (let ((itemid (lj-header-content "x-lj-itemid")))
      (and (zerop (length itemid))
	   (error "ItemID missing, CANNOT delete this post from SXEmacs/LJ"))
      (goto-char (point-min))
      (make-extent (point) (point-at-eol))
      (lj-update-userpic-glyph
       (expand-file-name (lj-header-content "userpic")
			 lj-userpic-directory))
      (re-search-forward lj-header-separator nil t)
      (forward-line -1)
      (set-extent-property
       (make-extent (point-at-bol) (1+ (point-at-eol))) 'invisible t)
      (lj-goto-body)
      (lj-mode)
      (and (y-or-n-p "Are you sure you want to delete this post? ")
	   (progn
	     (delete-file post)
	     (lj-delete-post-internal itemid))))))

(provide 'lj)

;; On-load actions
(and (file-exists-p lj-tags-file)
     (load-file lj-tags-file))
(and (file-exists-p lj-groups-file)
     (load-file lj-groups-file))
(and (file-exists-p lj-moods-file)
     (load-file lj-moods-file))
(and (file-exists-p lj-pickws-file)
     (load-file lj-pickws-file))
;;; lj.el ends here
