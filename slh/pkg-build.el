;;; pkg-build.el --- Automate building XEmacs packages for release.

;; Copyright (C) 2002 Steve Youngs

;; RCS: $Id: pkg-build.el,v 1.26 2004/03/15 22:55:33 youngs Exp $
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-04-07
;; Last-Modified: <2004-03-16 08:14:03 (steve)>
;; Keywords:      maint packages

;; This file is part of XEmacs

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA


;;; Commentary:
;;
;;  This is far from finished.  The intention here is to automate as
;;  much as possible the work of the XEmacs Package Release Manager.

;;; Currently implemented features:
;;
;;     - Increment Makefile $VERSION.
;;     - Write ChangeLog entry for incremented Makefile $VERSION.
;;     - Write ChangeLog entry in toplevel directory listing released
;;       packages.
;;     - Commit changed files to CVS.
;;     - Build single or multiple packages,
;;     - Do a 'cvs tag pkgname-x_xx' for each package built.
;;     - GnuPG sign package-index file.
;;     - Upload packages to FTP site.
;;     - Handle Sumo packages.  Building, tagging, uploading.
;;     - Keep a "packages to be released later" file
;;     - Release packages using above file as input.
;;
;;##################### W A R N I N G ################################
;;                                                         
;;              DANGER Will Robinson! DANGER!!
;;
;; This will *NOT* run out of the box.  You *MUST* customise it to
;; suit first.  And while on the subject of warnings... If you don't
;; have commit access to the entire packages CVS repository, this file
;; is not for you.  If you don't have an account on gwyn.tux.org, this
;; file is not for you.  If you are not the XEmacs Packages Release
;; Manager, this file is not for you.  Pretty God damned restrictive,
;; huh.
;;
;;####################################################################
;;
;; OK, warnings over, now Setup and Usage:
;;
;;   1) Stick this file somewhere in your load-path and byte-compile
;;      it.  And then add: (require 'pkg-build) to ~/.xemacs/init.el
;;      (or similar).  A better way to do this is left as an exercise
;;      for the reader.
;;
;;   2) Run 'M-x pkg-customize'  (remember those big fat warnings?)
;;
;;   3) The only functions you need to worry about are:
;;      - `pkg-release-packages'
;;      - `pkg-release-packages-from-file'
;;      - `pkg-release-packages-later'
;;      - `pkg-release-sumo'
;;
;; A Note About GnuPG Comments:
;;
;; Since this file switched over to using PGG instead of gpg.el,
;; setting your GnuPG comment field in this file is no longer
;; supported.  If you want to set the comment field, do so in your
;; `~/.gnupg/gpg.conf' file: `comment "Comment Here"'.

;;; Todo:
;;
;;     - Automated release announcements.
;;     - Lots and lots of code clean up.
;;

;;; History:
;;
;;  Go look at the ChangeLog, this is just here so `pkg-commentary'
;;  works.


;;; Code:
(eval-and-compile
  (require 'add-log)
  (require 'dired)
  (require 'pgg)
  (require 'package-get)
  (autoload 'lm-commentary "lisp-mnt")
  (autoload 'customize-group "cus-edit")
  (autoload 'with-electric-help "ehelp"))


(defconst pkg-build-version "3.0.5"
  "The version of pkg-build.")

(defgroup pkg-build nil
  "Preparing XEmacs packages for release."
  :prefix "pkg-"
  :group 'package-tools)

(defgroup pkg-directory nil
  "The directories associated with building packages."
  :prefix "pkg-"
  :group 'pkg-build)

(defgroup pkg-remote nil
  "Remote host options associated with building packages."
  :prefix "pkg-"
  :group 'pkg-build)

(defgroup pkg-sumo nil
  "Sumo options."
  :prefix "pkg-"
  :group 'pkg-build)

(defcustom pkg-std-packages
  '("Sun" "ada" "apel" "auctex" "bbdb" "build" "c-support" "calc"
    "calendar" "cc-mode" "clearcase" "cookie" "crisp" "debug" "dictionary"
    "dired" "docbookide" "ecb" "ecrypto" "edebug" "ediff" "edit-utils"
    "edt" "efs" "eieio" "elib" "emerge" "erc" "escreen" "eshell" "ess" 
    "eterm" "eudc" "footnote" "forms" "fortran-modes" "frame-icon" "fsf-compat"
    "games" "general-docs" "gnats" "gnus" "haskell-mode" "hm--html-menus"
    "hyperbole" "ibuffer" "idlwave" "igrep" "ilisp" "ispell" "jde" "liece"
    "mail-lib" "mailcrypt" "mew" "mh-e" "mine" "misc-games" "mmm-mode"
    "net-utils" "ocaml" "oo-browser" "os-utils" "pc" "pcl-cvs" "pcomplete"
    "perl-modes" "pgg" "prog-modes" "ps-print" "psgml" "psgml-dtds" 
    "python-modes" "reftex" "riece" "rmail" "ruby-modes" "sasl" "scheme"
    "semantic" "sgml" "sh-script" "sieve" "slider" "sml-mode" "sounds-au"
    "sounds-wav" "speedbar" "strokes" "supercite" "texinfo" "text-modes"
    "textools" "time" "tm" "tooltalk" "tpu" "tramp" "vc" "vc-cc" "vhdl"
    "view-process" "viper" "vm" "w3" "x-symbol" "xemacs-base" "xemacs-devel"
    "xlib" "xslide" "xslt-process" "xwem" "zenirc")
  "*A list of all standard (non-Mule) packages."
  :group 'pkg-build
  :type '(repeat string)
  :tag "Standard Packages"
  :link '(info-link "(xemacs)Available Packages"))

(defcustom pkg-mule-packages
  '("edict" "egg-its" "latin-unity" "leim" "locale" "lookup"
    "mule-base" "mule-ucs" "skk")
  "*A list of all Mule packages."
  :group 'pkg-build
  :type '(repeat string)
  :link '(info-link "(xemacs)Available Packages"))

(defcustom pkg-log-buffer "*Pkg Build Log*"
  "*The buffer that logs all the goings on from building packages."
  :group 'pkg-build
  :type 'string)

(defcustom pkg-log-entry-inc-ver "\"Increment VERSION in Makefile\""
  "*CVS commit log entry.

It is used when committing a package's Makefile & ChangeLog after
incrementing $VERSION."
  :group 'pkg-build
  :type 'string)

(defcustom pkg-copy-cmd "cp -a"
  "*The command used to copy a package into the working directory.

This command MUST do a recursive copy."
  :group 'pkg-build
  :type 'string)

(defcustom pkg-copy-glob "*"
  "*A shell wildcard that means \"All files in the directory\".

This is used in conjuction with `pkg-copy-cmd'."
  :group 'pkg-build
  :type '(choice 
	  (const :tag "GNU/Linux (\"*\")" "*")
	  (const :tag "FreeBSD (Nothing)" nil)
	  (string :tag "Other" "")))

(defcustom pkg-make-program (executable-find "gmake")
  "*The \"make\" program used to build packages.

The XEmacs packages need GNU/make to build."
  :group 'pkg-build
  :type '(file :must-match t))

(defcustom pkg-make-targets `((all    . ,(concat pkg-make-program " all"))
			      (aload  . ,(concat pkg-make-program " autoloads"))
			      (bdist  . ,(concat pkg-make-program " bindist"))
			      (clean  . ,(concat pkg-make-program " clean"))
			      (dclean . ,(concat pkg-make-program " distclean"))
			      (eclean . ,(concat pkg-make-program " extraclean"))
			      (html   . ,(concat pkg-make-program " html"))
			      (inst   . ,(concat pkg-make-program " install"))
			      (insth  . ,(concat pkg-make-program " install-html"))
			      (insto  . ,(concat pkg-make-program " install-only")))
  "*An alist of makefile targets used in building packages."
  :group 'pkg-build
  :type 'alist)

;;;###autoload
(defcustom pkg-packages-to-release-file
  (expand-file-name ".pkg-todo" (getenv "HOME"))
  "*A list of package names to be released at a later date."
  :group 'pkg-build
  :type 'file)

(defcustom pkg-clear-release-file t
  "*If non-nil, erase the contents of `pkg-packages-to-release-file'.

The file is cleared after the packages are released.  Defaults to
non-nil so that the same set of packages aren't released multiple
times with multiple release runs."
  :group 'pkg-build
  :type 'boolean)
	  
(defcustom pkg-base-directory
  (file-name-as-directory 
   (expand-file-name "programming/XEmacs/packages"
		     (getenv "HOME")))
  "*Base directory of the XEmacs packages source."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-std-directory
  (file-name-as-directory
   (expand-file-name "xemacs-packages" pkg-base-directory))
  "*Parent directory of standard packages."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-mule-directory
  (file-name-as-directory
   (expand-file-name "mule-packages" pkg-base-directory))
  "*Parent directory of Mule packages."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-working-base-directory
  (file-name-as-directory
   (expand-file-name "test-it/build/packages"
		     (getenv "HOME")))
  "*Base directory of the XEmacs packages source.

The `pkg-working-*-directory' are where the packages are actually
built.  I use a different directory from the checked out CVS source
just in case something goes wrong."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-working-std-directory
  (file-name-as-directory
   (expand-file-name "xemacs-packages" pkg-working-base-directory))
  "*Parent directory of standard packages.

The `pkg-working-*-directory' are where the packages are actually
built.  I use a different directory from the checked out CVS source
just in case something goes wrong."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-working-mule-directory
  (file-name-as-directory
   (expand-file-name "mule-packages" pkg-working-base-directory))
  "*Parent directory of Mule packages.

The `pkg-working-*-directory' are where the packages are actually
built.  I use a different directory from the checked out CVS source
just in case something goes wrong."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-staging-directory 
  (file-name-as-directory
   (expand-file-name "staging" (getenv "HOME")))
  "*This is the directory whereto make bindist puts its products."
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-upload-directory 
  (file-name-as-directory
   (expand-file-name "upload" (getenv "HOME")))
  "*This is the directory from where the packages get uploaded from.

WARNING! DANGER! *EVERYTHING* in this directory may deleted!  DON'T use
it for any other purpose.  You have been warned!!"
  :group 'pkg-directory
  :type 'directory)

(defcustom pkg-packages-omit-sumo '("Sun")
  "*A list of packages to NOT include in the SUMO packages."
  :group 'pkg-sumo
  :type '(repeat string))

(defcustom pkg-sumo-tarball-directory
  (file-name-as-directory
   (expand-file-name "Sumo"
		     pkg-staging-directory))
  "*The top level Sumo directory.

Under this directory is where all the packages are installed to in
readiness for creating the Sumo package tarballs."
  :group 'pkg-sumo
  :type 'directory)

(defcustom pkg-std-sumo-install-directory
  (file-name-as-directory
   (expand-file-name "xemacs-packages"
		     pkg-sumo-tarball-directory))
  "*The directory where the standard packages are installed for Sumo creation."
  :group 'pkg-sumo
  :type 'directory)

(defcustom pkg-mule-sumo-install-directory
  (file-name-as-directory
   (expand-file-name "mule-packages"
		     pkg-sumo-tarball-directory))
  "*The directory where the mule packages are installed for Sumo creation."
  :group 'pkg-sumo
  :type 'directory)

(defcustom pkg-std-sumo-build-flags
  (format "NONMULE_INSTALLED_PACKAGES_ROOT=%s"
	  pkg-std-sumo-install-directory)
  "*Args to pass to make when building the standard Sumo."
  :group 'pkg-sumo
  :type 'string)

(defcustom pkg-mule-sumo-build-flags
  (format "MULE_INSTALLED_PACKAGES_ROOT=%s"
	  pkg-mule-sumo-install-directory)
  "*Args to pass to make when building the Mule Sumo."
  :group 'pkg-sumo
  :type 'string)

(defcustom pkg-sumo-tar-gzip
  "tar --create --owner=0 --group=0 --use-compress-program=gzip --file"
  "*The program and flags used to build the tar.gz Sumo tarballs."
  :group 'pkg-sumo
  :type 'string)

(defcustom pkg-sumo-tar-bzip
  "tar --create --owner=0 --group=0 --use-compress-program=bzip2 --file"
  "*The program and flags used to build the tar.bz2 Sumo tarballs."
  :group 'pkg-sumo
  :type 'string)

(defcustom pkg-online-status-file "/var/run/ppp0.pid"
  "*This file must exist for CVS tagging and commits to happen."
  :group 'pkg-remote
  :type 'file)

(defcustom pkg-upload-command "scp -qC"
  "*Program to use for uploading packages & Sumo's to the FTP site."
  :group 'pkg-remote
  :type 'string)

(defcustom pkg-remote-path "youngs@ftp.xemacs.org:pkgtmp/"
  "*Path where packages get uploaded to.

This path is compatible with \"scp\" and includes a user name."
  :group 'pkg-remote
  :type 'string)

;;;###autoload
(defun pkg-customize ()
  "Convenience function to customise pkg-build."
  (interactive)
  (customize-group 'pkg-build))

;;;###autoload
(defun pkg-version (&optional arg)
  "Print the current version of pkg-build.

With prefix arg ARG, insert version string at point."
  (interactive "P")
  (if arg
      (insert (format "Pkg Build v%s" pkg-build-version))
    (message (format "Pkg Build v%s" pkg-build-version))))

;;;###autoload
(defun pkg-commentary ()
  "Display the commentary section from pkg-build.el."
  (interactive)
  ;; Yeah, yeah, I know there is `finder-commentary', but this is
  ;; better.
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert (lm-commentary (locate-library "pkg-build.el")))
	 (goto-char (point-min))
	 (while (re-search-forward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))))

(defsubst pkg-call-process (command &optional buffer displayp)
  "Call a shell process executing COMMAND with output to BUFFER.

Argument COMMAND is a string.
Optional argument BUFFER if omitted will be the current buffer.
Optional argument DISPLAYP if non-nil update the output buffer on the
fly instead of just when command finishes."
  (apply 'call-process shell-file-name nil (or buffer t) displayp
	 shell-command-switch (list command)))

(defun pkg-make-bindist (pkg type)
  "Build a binary distribution for package PKG.

Argument TYPE is a string of either \"standard\" or \"mule\".

This function is called from `pkg-release-packages'.  It copies the
package's source files from the CVS checkout directory to a working
directory.  And then does a \"make distclean\", \"make autoloads\",
and a \"make bindist\".

The output from the compilations is appended to `pkg-log-buffer'."
  (let (copy-dest)
    ;; Mule or Standard.
    (cond
     ((string= type "standard")
      (setq default-directory 
	    (file-name-as-directory 
	     (expand-file-name pkg pkg-std-directory))
	    copy-dest
	    (file-name-as-directory
	     (expand-file-name pkg pkg-working-std-directory))))
     ((string= type "mule")
      (setq default-directory 
	    (file-name-as-directory 
	     (expand-file-name pkg pkg-mule-directory))
	    copy-dest
	    (file-name-as-directory
	     (expand-file-name pkg pkg-working-mule-directory))))
     (t
      (error 'wrong-type-argument
	     (format "Type: %s is not valid for package: %s" type pkg))))
    ;; Copy the package to the working directory
    (shell-command
     (concat
      pkg-copy-cmd " " default-directory pkg-copy-glob " " copy-dest))
    ;; Set the default directory to the working directory
    (setq default-directory copy-dest)
    ;; make distclean.
    (pkg-call-process 
     (cdr (assq 'dclean pkg-make-targets)) pkg-log-buffer t)
    ;; make autoloads.
    (pkg-call-process 
     (cdr (assq 'aload pkg-make-targets)) pkg-log-buffer t)
    ;; make all
    (pkg-call-process
     (cdr (assq 'all pkg-make-targets)) pkg-log-buffer t)
    ;; make bindist.
    (pkg-call-process
     (cdr (assq 'bdist pkg-make-targets)) pkg-log-buffer t)))

(defun pkg-commit-inc-ver (pkg type)
  "Commit the ChangeLog & Makefile of package PKG.

Argument TYPE is a string of either \"standard\" or \"mule\".

Once $VERSION in Makefile has been incremented and a ChangeLog entry
written, this function commits the Makefile & ChangeLog to CVS."
  (let ((files "ChangeLog Makefile")
	(cvs-cmd "cvs commit -m"))
    ;; Mule or Standard.
    (cond
     ((string= type "standard")
      (setq default-directory 
	    (file-name-as-directory (expand-file-name pkg pkg-std-directory))))
     ((string= type "mule")
      (setq default-directory 
	    (file-name-as-directory (expand-file-name pkg pkg-mule-directory))))
     (t
      (error 'wrong-type-argument
	     (format "Type: %s is not valid for package: %s" type pkg))))
    (pkg-call-process
     (concat cvs-cmd " " pkg-log-entry-inc-ver " " files)
     pkg-log-buffer t)))

(defun pkg-tag-package (pkg version type)
  "Run \"cvs tag package-x_xx\" for package PKG.

Argument PKG is the name of the package to tag (a string).
Argument VERSION is the version number of the package to tag.
Argument TYPE is a string of either \"standard\" or \"mule\"."
  (let ((tag-name))
    (with-temp-buffer
      (erase-buffer)
      (insert (concat pkg "-" (format "%.2f" version)))
      (while (re-search-backward "\\." nil t)
	(replace-match "_" nil nil))
      (goto-char (point-min))
      (re-search-forward ".*$" (eolp) t)
      (setq tag-name (match-string 0)))
    (cond
     ((string= type "standard")
      (setq default-directory
	    (file-name-as-directory (expand-file-name pkg pkg-std-directory)))
      (pkg-call-process (concat "cvs tag " tag-name) pkg-log-buffer t))
     ((string= type "mule")
      (setq default-directory
	    (file-name-as-directory (expand-file-name pkg pkg-mule-directory)))
      (pkg-call-process (concat "cvs tag " tag-name) pkg-log-buffer t))
     (t
      (error 'wrong-type-argument
	     (format "Type: %s is not valid for package: %s" type pkg))))))

(defun pkg-upload-packages (pkgs)
  "Upload compiled XEmacs packages to the FTP site.

Argument PKGS is a list of filenames to upload."
  (let* ((staging (file-name-as-directory pkg-staging-directory))
	 (oldfilelist (directory-files pkg-upload-directory nil nil nil t))
	 (pgg-output-buffer "package-index.gpg"))
    (setq default-directory staging)
    (loop for each in '("package-index"
			"package-index.gpg")
      do (find-file-noselect each))
    (erase-buffer "package-index.gpg")
    (with-current-buffer "package-index"
      (pgg-sign-region (point-min) (point-max) 'cleartext)
      (kill-buffer (current-buffer)))
    (with-current-buffer pgg-output-buffer
      (save-buffer)
      (kill-buffer (current-buffer)))
    (loop for each in '("package-index"
			"package-index.gpg"
			"setup-packages.ini")
      do (setq pkgs (push each pkgs)))
    ;; DANGER Will Robinson! Delete anything that is in
    ;; `pkg-upload-directory'.
    (if (yes-or-no-p "DELETE upload/* BEFORE we copy files into it? ")
	(let* ((default-directory pkg-upload-directory))
	  (while oldfilelist
	    (delete-file (car oldfilelist))
	    (setq oldfilelist (cdr oldfilelist)))))
    (while pkgs
      (copy-file (expand-file-name (car pkgs) staging) pkg-upload-directory)
      (setq pkgs (cdr pkgs)))
    (start-process-shell-command "Package Upload"
				 pkg-log-buffer
				 (concat pkg-upload-command " "
					 (expand-file-name "*"
							   pkg-upload-directory)
					 " "
					 pkg-remote-path))))

;;;###autoload
(defun pkg-release-packages (packages)
  "Prepare PACKAGES for release.

Argument PACKAGES is a list of one or more XEmacs packages to
operate on.  They must be members of either `pkg-std-packages' or
`pkg-mule-packages'.

It increments $VERSION in the Makefile and writes a new ChangeLog
entry, commits those changes to CVS, and start the ball rolling on
tagging the CVS repository, building the binary distribution, and
upload them to the FTP site."
  (interactive "sWhitespace separated list of 1 or more packages: ")
  (when (not (featurep 'mule))
    (error 'unimplemented
	   "`mule': It is best to build packages with a Mule-enabled XEmacs"))
  (let* ((pkg-list (with-temp-buffer
		     (insert packages)
		     (remove-if (lambda (s) (string= "" s))
				(split-string 
				 (buffer-string (current-buffer))))))
	 (changelog (expand-file-name "ChangeLog" pkg-base-directory))
	 (log-entry "\"Package release\"")
	 (ci-top-changelog (concat "cvs commit -m "
				   log-entry " ChangeLog"))
	 packages type tarball-name all-tarballs)
    (setq packages pkg-list)
    (switch-to-buffer (get-buffer-create pkg-log-buffer))
    (insert
     (format "Releasing the following packages:\n\t%s" packages))
    (insert "\n===================================================\n")
    ;; Add a ChangeLog entry in the top level ChangeLog and commit
    ;; that to CVS if we're online.
    (save-excursion
      (find-file changelog)
      (add-change-log-entry nil nil nil t)
      (insert
       (format "Packages released: %s." packages))
      (while (re-search-backward "(\\|)" nil t)
	(replace-match "" nil nil))
      (fill-paragraph 1)
      (change-log-exit))
    (if (file-readable-p pkg-online-status-file)
	(progn
	  (setq default-directory pkg-base-directory)
	  (pkg-call-process ci-top-changelog pkg-log-buffer t))
      (save-excursion
	(set-buffer pkg-log-buffer)
	(insert-string "\n*** OFFLINE *** Commit toplevel ChangeLog manually.\n")))
    ;; Increment $VERSION for each package and write a ChangeLog entry
    ;; for it.
    (while packages
      ;; Standard or Mule.
      (cond
       ((member (car packages) pkg-std-packages)
	(set-buffer (find-file (expand-file-name 
				"Makefile"
				(file-name-as-directory
				 (expand-file-name (car packages)
						   pkg-std-directory)))))
	(setq type "standard"))
       ((member (car packages) pkg-mule-packages)
	(set-buffer (find-file (expand-file-name 
				"Makefile"
				(file-name-as-directory
				 (expand-file-name (car packages)
						   pkg-mule-directory)))))
	(setq type "mule"))
       (t
	(error 'invalid-argument
	       (format "%s is not a valid package" (car packages)))))
      (goto-char (point-min))
      (re-search-forward "^VERSION = " nil t)
      (re-search-forward ".*$" (eolp) t)
      (let* ((oldver (match-string 0))
	     (incver (+ 0.01 (string-to-number oldver))))
	(replace-match (format "%.2f" incver))
	(save-buffer (current-buffer))
	(add-change-log-entry nil nil nil t)
	(insert
	 (format "XEmacs package %.2f released." incver))
	(change-log-exit)
	(kill-buffer (current-buffer))
	(pkg-make-bindist (car packages) type)
	(setq tarball-name (concat (car packages) "-"
				   (format "%.2f" incver)
				   "-pkg.tar.gz"))
	(setq all-tarballs (push tarball-name all-tarballs))
	;; If online, commit the Makefile & ChangeLog, and tag CVS.
	(if (file-readable-p pkg-online-status-file)
	    (progn
	      (pkg-commit-inc-ver (car packages) type)
	      (pkg-tag-package (car packages) incver type))
	  (save-excursion
	    (set-buffer pkg-log-buffer)
	    (insert
	     (format
	      "\n*** OFFLINE *** Commit changes and tag tree for %s manually.\n"
	      (car packages)))))
      (setq packages (cdr packages))))
    ;; If online, upload the package tarballs.
    (if (file-readable-p pkg-online-status-file)
	(pkg-upload-packages all-tarballs)
      (save-excursion
	(set-buffer pkg-log-buffer)
	(insert
	 (format
	  "\n*** OFFLINE *** Upload these packages manually:\n\t%s"
	  (cl-prettyprint (symbol-value 'all-tarballs))))))))
  
;;;###autoload
(defun pkg-release-packages-from-file (&optional file arg)
  "Call `pkg-release-packages' using FILE as input.

Optional Argument FILE is a file to use.  It must contain a single
line that is a whitespace separated list of package names.

With Prefix Argument ARG, prompt for a file to use.

The default is to use `pkg-packages-to-release-file', but if that
doesn't exist, prompt for a file to use."
  (interactive "i\nP")
  (let* ((list-file (if file
			file
		      (if (or arg
			      (not (file-exists-p pkg-packages-to-release-file)))
			  (expand-file-name
			   (read-file-name "Package list file: " nil nil t))
			pkg-packages-to-release-file)))
	 (buf (find-file-noselect list-file))
	 (pkg-list (buffer-string buf)))
    (when (not (file-readable-p list-file))
      (error 'file-error 
	     "Release file not readable or nonexistent"))
    (pkg-release-packages pkg-list)
    (save-excursion
      (switch-to-buffer buf)
      (when pkg-clear-release-file
	(erase-buffer buf))
      (save-buffer)
      (kill-buffer buf))))

;;;###autoload
(defun pkg-release-packages-later (packages &optional now)
  "Create or add to a list of packages to release.

The list is kept in the file, `pkg-packages-to-release-file'.

Argument PACKAGES is a whitespace separated list of package names.

With Optional prefix Argument NOW, release the packages instead
of keeping a list to do later."
  (interactive "sWhitespace separated list of packages: \nP")
  (if now
      (pkg-release-packages packages)
    (let ((buf (find-file-noselect pkg-packages-to-release-file)))
      (save-excursion
	(switch-to-buffer (get-buffer buf))
	(when (y-or-n-p "Erase any previous entries first? ")
	  (erase-buffer buf))
	(insert (concat packages " "))
	(save-buffer)
	(kill-buffer buf)))))

;;;###autoload
(defun pkg-release-sumo ()
  "Builds the SUMO packages."
  (interactive)
  (when (not (featurep 'mule))
    (error 'unimplemented
	   "`mule': Sumos built with a non-Mule XEmacs is discouraged"))
  (switch-to-buffer (get-buffer-create pkg-log-buffer))
  (let ((sumo-std (reverse pkg-std-packages))
	(sumo-mule pkg-mule-packages)
	(sumo-pkgs))
    (while sumo-std
      (setq sumo-pkgs (push (car sumo-std) sumo-mule))
      (setq sumo-std (cdr sumo-std)))
    ;; Do a 'make autoloads' just to be on the safe side.
    (setq default-directory pkg-working-base-directory)
    (pkg-call-process 
     (cdr (assq 'aload pkg-make-targets)) pkg-log-buffer t)
    ;; Build each package.
    (while sumo-pkgs
      (unless (member (car sumo-pkgs) pkg-packages-omit-sumo)
	(cond
	 ((member (car sumo-pkgs) pkg-std-packages)
	  (setq default-directory (file-name-as-directory
				   (expand-file-name (car sumo-pkgs)
						     pkg-working-std-directory)))
	  (pkg-call-process 
	   (cdr (assq 'all pkg-make-targets)) pkg-log-buffer t)
	  (pkg-call-process 
	   (concat (cdr (assq 'inst pkg-make-targets))
		   " "
		   pkg-std-sumo-build-flags) pkg-log-buffer t))
	 ((member (car sumo-pkgs) pkg-mule-packages)
	  (setq default-directory (file-name-as-directory
				   (expand-file-name (car sumo-pkgs)
						     pkg-working-mule-directory)))
	  (pkg-call-process 
	   (cdr (assq 'all pkg-make-targets)) pkg-log-buffer t)
	  (pkg-call-process 
	   (concat (cdr (assq 'inst pkg-make-targets))
		   " "
		   pkg-mule-sumo-build-flags) pkg-log-buffer t))
	 (t
	  (error 'invalid-argument
		 (format "%s is not a valid package" (car sumo-pkgs))))))
      (setq sumo-pkgs (cdr sumo-pkgs))))
  ;; Add a ChangeLog entry to say that a Sumo package has been released.
  (save-excursion
    (find-file (expand-file-name "ChangeLog" pkg-base-directory))
    (add-change-log-entry nil nil nil t)
    (insert-string "Sumo packages released.")
    (change-log-exit))
  (let ((log-msg "\"Sumo packages released\"")
	(tag (format-time-string "sumo-%Y-%m-%d"))
	(sumo-build-directory pkg-sumo-tarball-directory))
    (if (file-readable-p pkg-online-status-file)
	(progn
	  ;; Commit the ChangeLog.
	  (setq default-directory pkg-base-directory)
	  (pkg-call-process (concat "cvs commit -m "
				    log-msg
				    " ChangeLog")
			    pkg-log-buffer t)
	  ;; Tag the tree.
	  (pkg-call-process (concat "cvs tag " tag) pkg-log-buffer t)
	  (pkg-call-process "cvs tag -F sumo-current" pkg-log-buffer t))
      (save-excursion
	(set-buffer pkg-log-buffer)
	(insert
	 (format
	  "\n*** OFFLINE *** Manually commit ChangeLog and tag tree as: %s.\n"
	  tag))))
    ;; Build the tarballs.
    (setq default-directory sumo-build-directory)
    (pkg-call-process (concat pkg-sumo-tar-gzip " "
			      (format-time-string
			       "./xemacs-sumo-%Y-%m-%d.tar.gz")
			       " ./xemacs-packages/")
		      pkg-log-buffer t)
    (pkg-call-process (concat pkg-sumo-tar-gzip " "
			      (format-time-string
			       "./xemacs-mule-sumo-%Y-%m-%d.tar.gz")
			       " ./mule-packages/")
		      pkg-log-buffer t)
    (pkg-call-process (concat pkg-sumo-tar-bzip " "
			      (format-time-string
			       "./xemacs-sumo-%Y-%m-%d.tar.bz2")
			       " ./xemacs-packages/")
		      pkg-log-buffer t)
    (pkg-call-process (concat pkg-sumo-tar-bzip " "
			      (format-time-string
			       "./xemacs-mule-sumo-%Y-%m-%d.tar.bz2")
			       " ./mule-packages/")
		      pkg-log-buffer t)
    ;; Upload them if we're online.
    (if (file-readable-p pkg-online-status-file)
	(start-process-shell-command "Sumo Upload"
				     pkg-log-buffer
				     (concat pkg-upload-command " "
					     "*-sumo-*.tar.* "
					     pkg-remote-path))
      (save-excursion
	(set-buffer pkg-log-buffer)
	(insert-string "\n*** OFFLINE *** Upload Sumo tarballs manually.\n")))))

(provide 'pkg-build)

;;; pkg-build.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
