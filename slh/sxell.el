;;; sxell.el --- Browse the Emacs Lisp List (SXEmacs version)

;; Copyright (C) 2005 Steve Youngs

;; Author:         Steve Youngs <steve@sxemacs.org>
;; Maintainer:     Steve Youngs <steve@sxemacs.org>
;; Created:        Jul 4, 2005
;; Version:        0.1
;; Download:       ftp://ftp.youngs.au.com/pub/lisp/SXEmacs/sxell.el

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
;;   The Emacs Lisp List is a list of links to a wide variety of
;;   emacs-lisp libraries around the globe.  The list is maintained by
;;   Stephen Eglen and can be found at
;;   http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html.  sxell.el
;;   allows you to view that list in a normal buffer inside SXEmacs
;;   (no, sxell.el does NOT work with XEmacs or GNU/Emacs).
;;
;;   The data you see in the *sxell-packages* buffer originally comes
;;   from a .xml file from Stephen's site.  This .xml is parsed and
;;   stored in a local PostgreSQL db.  Most operations default to
;;   using the local PostgreSQL db.  Remote operations are only done
;;   when explicitly requested.
;;
;;   When checking for updates, to save on bandwidth, just the HTTP
;;   header of the .xml file is downloaded and a match against the
;;   "Etag" header is done. (this is also saved in the db).  If the
;;   Etag hasn't changed, the .xml file isn't downloaded.
;;
;;   Keeping a copy of the list in a local db allows for, amongst
;;   other things, "offline" operation and complex searching of the
;;   list.
;;
;;   The idea for this comes from ell.el by Jean-Philippe Theberge et
;;   al.

;;; Code:

(unless (featurep 'sxemacs)
  (error "We're sorry, this library is for SXEmacs ONLY"))

(require 'xml)

(when (fboundp 'ffi-defun)
  (require 'ffi-curl))

(unless (featurep '(and ffi postgresql))
  (error 'unimplemented "FFI and/or PostgreSQL"))

(defgroup sxell nil
  "Browse the Emacs Lisp List."
  :prefix "sxell-"
  :group 'hypermedia)

(defcustom sxell-initialised-flag nil
  "*When nil, initialise the PostgreSQL db and import the Ell.

This variable is set to non-nil and saved the first time you run
`sxell-packages', so in most cases you can leave this alone."
  :type 'boolean
  :group 'sxell)

(defcustom sxell-remote-file 
  "http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.xml"
  "*URL to the Emacs Lisp List XML file."
  :type 'string
  :group 'sxell)

(defcustom sxell-local-file (expand-file-name "ell.xml" (temp-directory))
  "*Local version of the Emacs Lisp List XML file."
  :type 'string
  :group 'sxell)

(defcustom sxell-download-directory
  (file-name-as-directory (user-home-directory))
  "*Directory for files downloaded from the *sxell-packages* buffer.

The default is $HOME."
  :type '(directory :must-match t)
  :group 'sxell)

(defcustom sxell-use-font-lock t
  "*If non-nil, we font-lock the ELL buffer."
  :type 'boolean
  :group 'sxell)

(defcustom sxell-mode-hook nil
  "*Hooks run after entering `sxell-mode'."
  :type 'hook
  :group 'sxell)

(defcustom sxell-download-hook nil
  "*Hooks run after downloading a file from the *sxell-packages* buffer."
  :type 'hook
  :group 'sxell)

(defcustom sxell-fetch-remote-xml-hook nil
  "*Hooks run after fetching the ELL xml file."
  :type 'hook
  :group 'sxell)

(defvar sxell-last-updated nil
  "Date that the list was last updated.

Internal var, don't set it.")

(defvar sxell-current-etag nil
  "ETag HTTP header of ell.xml.

Internal var, don't set it.")

(defun sxell-db-initialise ()
  "Initialise the SXEll PostgreSQL database."
  (let* ((initdb (pq-connectdb "dbname=template1"))
	 (ellres (pq-exec 
		  initdb 
		  "SELECT * FROM pg_catalog.pg_database WHERE datname='ell';")))
    (when (zerop (pq-ntuples ellres))
      (pq-exec initdb "CREATE DATABASE ell;"))
    (pq-finish initdb))
  (let* ((elldb (pq-connectdb "dbname=ell")))
    (ignore-errors
      (pq-exec elldb "CREATE TABLE ell ( 
filename text NOT NULL DEFAULT ''::text, 
description text NOT NULL DEFAULT ''::text, 
site text NOT NULL DEFAULT 'http://'::text, 
contact text NOT NULL DEFAULT ''::text, 
time_stamp date NOT NULL DEFAULT ('now'::text)::date, 
note text NOT NULL DEFAULT ''::text, 
installed_p bool NOT NULL DEFAULT false, 
direct_link_p bool );"))
    (ignore-errors
      (pq-exec elldb "CREATE TABLE last_upd (
etag text NOT NULL DEFAULT ''::text,
last_date text NOT NULL DEFAULT ''::text );")
      (pq-exec elldb "INSERT INTO last_upd VALUES (
'bogus-etag','Thu Jan 1 00:00:00 EST 1970' );"))
    (pq-finish elldb)))

(defun sxell-get-pg-packages-list (&optional sql)
  "Return a list of packages from the local PostgreSQL db.

Optional argument, SQL is the PostgreSQL SELECT statement to use.  If
it is omitted, `SELECT * FROM ell ORDER by filename ;' is used."
  (let* ((db (pq-connectdb "dbname=ell"))
	 (res (pq-exec db (or sql "SELECT * FROM ell ORDER by filename ;")))
	 (nrows (pq-ntuples res))
	 (nfields (pq-nfields res))
	 (upd-res (pq-exec db "SELECT last_date FROM last_upd ;"))
	 list-o-matic mega-list)
    (loop for row from 0 to (1- nrows)
      do (loop for field downfrom (1- nfields) to 0
	   do (push (pq-get-value res row field) list-o-matic))
      do (push list-o-matic mega-list)
      do (setq list-o-matic nil))
    (setq sxell-last-updated (pq-get-value upd-res 0 0))
    (pq-finish db)
    (nreverse mega-list)))

(defun sxell-fetch-ell-xml ()
  (message "Fetching Emacs Lisp List.  Please wait...")
  (curl:download sxell-remote-file sxell-local-file)
  (message "Fetching Emacs Lisp List.  Done!")
  (run-hooks 'sxell-fetch-remote-xml-hook))

(defun sxell-fetch-ell-etag ()
  "Returns the \"ETag\" of ell.xml's HTTP header."
  (let ((file (expand-file-name "ell.tag" (temp-directory)))
	etag)
    (curl:download sxell-remote-file file :header t :nobody t)
    (setq etag (with-temp-buffer
		 (insert-file-contents-literally file)
		 (goto-char (point-min))
		 (re-search-forward "^ETag:\\s-\"\\(.*\\)\"" nil t)
		 (match-string 1)))
    (delete-file file)
    etag))

(defun sxell-parse-ell-xml ()
  "Parse the contents of the ELL site ell.xml file."
  (let* ((xml (xml-parse-file sxell-local-file))
         (root (car xml))
         (entries (cadddr root)))
    (setq sxell-last-updated (nth 2 (caddr root)))
    (mapcar (lambda (entry)
	      (let ((attrs (cadr entry))) 
		(list 
		 (pq-escape-string (cdr (assoc 'filename attrs)))
		 (pq-escape-string (cdr (assoc 'description attrs)))
		 (pq-escape-string (cdr (assoc 'site attrs)))
		 (pq-escape-string (cdr (assoc 'contact attrs)))
		 (pq-escape-string (cdr (assoc 'timestamp attrs)))
		 (pq-escape-string (cdr (assoc 'note attrs))))))
	    (cddr entries))))

(defun sxell-update-pg-from-xml ()
  (let ((entries (sxell-parse-ell-xml))
	(db (pq-connectdb "dbname=ell"))
	(chk-entry-fmt (concat "SELECT * FROM ell WHERE "
			       "filename = '%s' AND ( description = '%s' OR "
			       "site = '%s' OR contact = '%s' OR "
			       "time_stamp = '%s' OR note = '%s' ) ;"))
	(upd-fmt (concat "UPDATE ell "
			 "SET filename = '%s', description = '%s', "
			 "site = '%s', contact = '%s', time_stamp = '%s', "
			 "note = '%s' "
			 "WHERE filename = '%1$s' AND ( description = '%2$s' "
			 "OR site = '%3$s' OR contact = '%4$s' OR "
			 "time_stamp = '%5$s' OR note = '%6$s' ) ;"))
	existing)
    (while entries
      ;; Check to see if we need to update or add an entry.
      (setq existing (pq-exec db (apply #'format chk-entry-fmt (car entries))))
      (if (zerop (pq-ntuples existing))
	  ;; This is a new entry
	  (pq-exec db (format "INSERT INTO ell VALUES (%s) ;"
			      (mapconcat #'(lambda (el)
					     (concat "'" el "'"))
					 (car entries) ",")))
	;; Existing entry, update it
	(pq-exec db (apply #'format upd-fmt (car entries))))
      (setq entries (cdr entries)))
    ;; Update last updated date and etag
    (pq-exec db (format "UPDATE last_upd SET last_date = '%s', etag = '%s' ;"
			sxell-last-updated sxell-current-etag))
    (pq-finish db)))

(defvar sxell-font-lock-keywords
  '((" <\\(New\\)> "  (1 font-lock-warning-face))
    ("^Note: \\(.*$\\)" (1 font-lock-warning-face))
    ("^\\(Note\\|Contact\\|Added\\):" (1 font-lock-keyword-face))
    ("^\\*" . font-lock-warning-face)
    ("^\\*?\\(\\w+.*\\(\\.el\\)?\\)\\s-\\(<\\|-\\)"
     (1 font-lock-function-name-face)))
  "Font lock keywords in sxell mode.")

(defun sxell-prepare-buffer ()
  "Prepare to make the new *sxell-packages* buffer."
  (switch-to-buffer (get-buffer-create "*sxell-packages*"))
  (erase-buffer)
  (insert "==========================================")
  (center-line)(insert "\n")
  (insert "The Emacs Lisp List")(center-line)(insert "\n")
  (insert "by Stephen Eglen: stephen@anc.ed.ac.uk")(center-line)(insert "\n")
  (insert "==========================================")
  (center-line)(insert "\n\n"))

(defun sxell-update-buffer (date)
  "Update the counters at the top of the *sxell-packages* buffer.
DATE is the date when ELL was last updated."
  (when sxell-last-updated
    (goto-line 5)
    (insert (format "Last updated: %s" date))
    (goto-line 5)
    (center-line)))

(defun sxell-url-at-point ()
  "Browse to a URL from the sxell buffer."
  (interactive)
  (when (extentp (extent-at (point)))
    (browse-url (extent-string (extent-at (point))))))

(defun sxell-url-at-mouse (event)
  "Browse to a URL at EVENT via the mouse from the sxell buffer."
  (interactive "e")
  (when (extentp (extent-at-event event))
    (browse-url (extent-string (extent-at-event event)))))

(defun sxell-download-file-at-point ()
  "Download the file from the URL in the sxell buffer."
  (interactive)
  (when (extentp (extent-at (point)))
    (let* ((remote (extent-string (extent-at (point))))
	   (local (car (last (split-string-by-char remote ?/)))))
      (if (string-match ".*\\.\\(el\\|t?gz\\|bz2\\)$" local)
	  (curl:download remote
			 (expand-file-name local sxell-download-directory))
	(message "Nothing to download here :-(")))))

(defun sxell-download-file-at-mouse (event)
  "Download the file from the URL in the sxell buffer."
  (interactive "e")
  (when (extentp (extent-at-event event))
    (let* ((remote (extent-string (extent-at-event event)))
	   (local (car (last (split-string-by-char remote ?/)))))
      (if (string-match ".*\\.\\(el\\|t?gz\\|bz2\\)$" local)
	  (curl:download remote
			 (expand-file-name local sxell-download-directory))
	(message-or-box "Nothing to download here :-(")))))

(defun sxell-kill-buffer ()
  (interactive)
  (kill-buffer nil)
  (when (file-exists-p sxell-local-file)
    (delete-file sxell-local-file)))

(defconst sxell-mode-map
  (let* ((map (make-sparse-keymap 'sxell-mode-map)))
    (define-key map [space] 'scroll-up)
    (define-key map [delete] 'scroll-down)
    (define-key map [q] 'bury-buffer)
    (define-key map [Q] 'sxell-kill-buffer)
    map)
  "A keymap for the sxell buffer.")

(defconst sxell-ext-map
  (let* ((map (make-sparse-keymap 'sxell-ext-map)))
    (define-key map [button2] 'sxell-url-at-mouse)
    (define-key map [return] 'sxell-url-at-point)
    (define-key map [d] 'sxell-download-file-at-point)
    (define-key map [(control button2)] 'sxell-download-file-at-mouse)
    map)
  "A keymap for the extents in sxell buffer.")

(defun sxell-make-url-extents ()
  "Create extent objects for all the URLs in the buffer."
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "^\\(ht\\|f\\)tp.*$" nil t)
      (let ((extent (make-extent (match-beginning 0) (match-end 0)))
	    (echo "Visit: RET, button2; Download: d, C-button2"))
	(set-extent-property extent 'face 'font-lock-comment-face)
	(set-extent-property extent 'mouse-face 'highlight)
	(set-extent-property extent 'keymap sxell-ext-map)
	(set-extent-property extent 'help-echo echo)
	(set-extent-property extent 'balloon-help echo)
	(set-extent-property extent 'duplicable t)))))

(defun sxell-fix-quoting ()
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "&quot;" nil t)
      (replace-match "\""))))

(defun sxell-mode ()
  "Major mode for browsing the Emacs Lisp List.
\\{sxell-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(sxell-font-lock-keywords t))
  (use-local-map sxell-mode-map)
  (setq major-mode 'sxell-mode
        mode-name "SXEll")
  (when sxell-use-font-lock
    (font-lock-mode))
  (run-hooks 'sxell-mode-hook))

(defun sxell-init ()
  "Initialise SXEll.

This _ONLY_ needs to be run _ONCE_.  It initialises the PostgreSQL
database, and fills it from a fresh copy of the ELL."
  (sxell-db-initialise)
  (customize-save-variable 'sxell-initialised-flag t)
  (sxell-packages 'remote))

(defun sxell-check-remote-update ()
  "Check to see if the local Ell db needs updating from the remote."
  (let* ((db (pq-connectdb "dbname=ell"))
	 (res (pq-exec db "SELECT etag FROM last_upd ;"))
	 old-etag)
    (setq old-etag (pq-get-value res 0 0))
    (setq sxell-current-etag (sxell-fetch-ell-etag))
    (pq-finish db)
    (unless (string= old-etag sxell-current-etag)
      (sxell-fetch-ell-xml)
      (sxell-update-pg-from-xml))))

(defun sxell-mark-installed ()
  "Mark ELL entries that are installed locally.

CAUTION: this can be a slow and CPU intensive operation, be patient."
  (interactive)
  (when (y-or-n-p "This can take considerable time, are you sure? ")
    (let* ((db (pq-connectdb "dbname=ell"))
	   (res
	    (pq-exec 
	     db
	     "SELECT DISTINCT filename FROM ell WHERE filename like '%.el' ;"))
	   (num (pq-ntuples res)))
      (message "Finding installed libraries... Please wait.")
      (loop for row from 0 to (1- num)
	do (pq-exec 
	    db
	    (format "UPDATE ell SET installed_p = '%s' WHERE filename = '%s';"
		    (if (locate-library (pq-get-value res row 0))
			"t"
		      "f")
		    (pq-get-value res row 0))))
      (pq-finish db)
      (message "Finding installed libraries... Done!"))))

(defun sxell-mark-downloadable ()
  "Mark ELL db entries that have a URL to a .el that can be directly downloaded.

For example: http://www.foo.com/foo.el"
  (interactive)
  (let ((db (pq-connectdb "dbname=ell")))
    (pq-exec db "UPDATE ell SET direct_link_p = 't' WHERE (
site LIKE '%.el' OR
site LIKE '%.gz' OR
site LIKE '%.bz2' OR
site LIKE '%.tgz' ) ;")
    (pq-exec db "UPDATE ell SET direct_link_p = 'f' WHERE (
site NOT LIKE '%.el' AND
site NOT LIKE '%.gz' AND
site NOT LIKE '%.bz2' AND
site NOT LIKE '%.tgz' ) ;")
    (pq-finish db)
    (message "Noted the directly downloadable files.")))

(defun sxell-sort-by-contact (&optional reverse remote)
  "Display ELL, sorted by contact.

The default is to display in alphabetical ascending order, using the
local data.  This behaviour can be changed by the use of prefix args:

    0 prefix arg  -- Sort ascending with local data \(default\)
    1 prefix args -- Sort descending with local data
    2 prefix args -- Sort ascending check remote updates
    3 prefix args -- Sort descending check remote updates

To do the same thing non-interactively, use:

Optional arg, REVERSE, display in reverse order.
Optional arg, REMOTE, check remote ELL for updates."
  (interactive "P")
  (let* ((arg current-prefix-arg)
	 ;; Reset `current-prefix-arg' to nil because `sxell-packages'
	 ;; can use a prefix arg too.
	 (current-prefix-arg nil)
	 (sql "SELECT * FROM ell ORDER by contact "))
    (if (not (interactive-p))
	;; When called non-interactively
	(progn
	  (setq sql (concat sql
			    (when reverse "desc ")
			    ";"))
	  (if remote
	      (sxell-packages 'remote sql)
	    (sxell-packages nil sql)))
      ;; When called interactively
      (cond
       ((eq (car arg) 4)
	(setq sql (concat sql "desc ;"))
	(sxell-packages nil sql))
       ((eq (car arg) 16)
	(setq sql (concat sql ";"))
	(sxell-packages 'remote sql))
       ((eq (car arg) 64)
	(setq sql (concat sql "desc ;"))
	(sxell-packages 'remote sql))
       (t
	(setq sql (concat sql ";"))
	(sxell-packages nil sql))))))

(defun sxell-sort-by-date (&optional oldfirst remote)
  "Display ELL, sorted by date.

The default is to display newest to oldest, using the local data.
This behaviour can be changed through the use of prefix args:

    0 prefix arg  -- newest to oldest with local data \(default\)
    1 prefix args -- oldest to newest with local data
    2 prefix args -- newest to oldest, check for remote updates
    3 prefix args -- oldest to newest, check for remote updates

To do the same thing non-interactively, use:

Optional arg, OLDFIRST, display oldest to newest.
Optional arg, REMOTE, check for remote updates."
  (interactive "P")
  (let* ((arg current-prefix-arg)
	 ;; Reset `current-prefix-arg' to nil because `sxell-packages'
	 ;; can use a prefix arg too.
	 (current-prefix-arg nil)
	 (sql "SELECT * FROM ell order by time_stamp "))
    (if (not (interactive-p))
	;; When called non-interactively
	(progn
	  (setq sql (concat sql
			    (unless oldfirst "desc ")
			    ";"))
	  (if remote
	      (sxell-packages 'remote sql)
	    (sxell-packages nil sql)))
      ;; When called interactively
      (cond
       ((eq (car arg) 4)
	(setq sql (concat sql ";"))
	(sxell-packages nil sql))
       ((eq (car arg) 16)
	(setq sql (concat sql "desc ;"))
	(sxell-packages 'remote sql))
       ((eq (car arg) 64)
	(setq sql (concat sql ";"))
	(sxell-packages 'remote sql))
       (t
	(setq sql (concat sql "desc ;"))
	(sxell-packages nil sql))))))

(defun sxell-search ()
  "Search records in ELL."
  (interactive)
  ;; write me... I'm thinking map-y-or-n-p shit
  )

(defun sxell-packages (&optional remote sql)
  "Display the Emacs Lisp List in a Emacs buffer.

The data for the list comes from the local PostgreSQL database.  The
first time this is run, the PostgreSQL database is initialised and the
Ell is imported into it.

With non-nil prefix arg, REMOTE, check for updates to the Ell.

Optional argument, SQL is the SQL SELECT statement to use.  If it is
omitted, `SELECT * FROM ell ORDER by filename ;' is used."
  (interactive "P")
  (unless sxell-initialised-flag
    (sxell-init))
  (when (or current-prefix-arg remote)
    (sxell-check-remote-update))
  (let ((packages (sxell-get-pg-packages-list sql)))
    (sxell-prepare-buffer)
    (insert "Files with an asterisk `*' "
	    "are already installed on your system.")
    (center-line)
    (insert "\n\n")
    (mapcar (lambda (x)
              ;; NAME - DESCRIPTION
	      ;; URL
              ;; Contact
	      ;; Added: TIMESTAMP Note: NOTE
              (let* ((name (car x))
                     (description (cadr x))
                     (url (caddr x))
		     (author (cadddr x))
		     (timestamp (car (cddddr x)))
                     (note (cadr (cddddr x)))
		     (installed (caddr (cddddr x))))
		     ;(downloadable (cadddr (cddddr x))))
                (insert (format "%s - %s\n%s\nContact: %s\nAdded: %s"
                                (if (string= installed "t")
                                    (concat "*" name)
                                  name)
                                description url author timestamp))
		(if (not (string= note ""))
		    (insert (format "\nNote: %s\n\n" note))
		  (insert "\n\n"))))
	    packages)
    (sxell-update-buffer sxell-last-updated)
    (sxell-mode)
    (sxell-make-url-extents)
    (sxell-fix-quoting)))


(provide 'sxell)
  
;;; sxell.el ends here
