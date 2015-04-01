;;;; dired-tar.el - extensions to dired to create and unpack tar files.

;;;; Originally by: Jim Blandy <jimb@cyclic.com> --- June 1995
;;;; Adapted to use bzip2 as well as gzip by Steve Youngs <steve@sxemacs.org>
;;;; Copyright (C) 1995 Jim Blandy
;;;; Copyright (C) 2005 - 2011 Steve Youngs

;; Author: Jim Blandy <jimb@cyclic.com>
;; Maintainer: Jim Blandy <jimb@cyclic.com>
;; Created: Mon  6 Sep 1993
;; Updated: Sun  3 Jul 2005
;; Version: 1.9
;; Keywords: unix

;;; Commentary:

;;; dired-tar adds a command to dired-mode for creating and unpacking
;;; tar files.  When using this package, typing `M-t' on a tar file in a
;;; dired listing unpacks the tar file, uncompressing it if necessary.
;;; Typing `M-t' on a directory packs up that directory into a gzipped,
;;; or bzip2'd tar file named DIRNAME.tar.gz (DIRNAME.tar.bz2 for bzip2).
;;;
;;; To use this package, just place it in a directory in your Emacs
;;; lisp load path, byte-compile it, and put the line
;;;    (require 'dired-tar)
;;; in your .emacs.
;;;
;;; This file defines the following function:
;;;
;;; dired-tar-pack-unpack - If the file on the current line is a tar
;;;    file, or a gzipped or compressed tar file, unpack it.  If the
;;;    file on the current line is a directory, build a tar file for
;;;    it, and gzip it.
;;;
;;; It also declares the following variables:
;;;
;;; dired-tar-compress-with - If the symbol `gzip', compress created tars 
;;;     with gzip, if `bzip2', compress with bzip2, if nil, don't compress 
;;;     tars.
;;;
;;; dired-tar-command-switches - flags to pass to the tar program.
;;;      This is concatenated with command characters ("x" or "c" or
;;;      whatever).  The default is 'vf'; I'm told Windows users
;;;      should use "mvf".
;;;
;;; dired-tar-gzip-extension - extension to use for gzipped tar files.
;;;      Defaults to ".tar.gz", but ".tgz" may be a useful value in
;;;      some circumstances.
;;;
;;; dired-tar-bzip2-extension - extension to use for bzipped tar files.
;;;      Defaults to ".tar.bz2", but ".tbz" or ".tbz2" may be a useful value in
;;;      some circumstances.
;;;
;;; dired-tar-gzip-command - a shell command which gzips its
;;;     standard input to its standard output.
;;;
;;; dired-tar-ungzip-command - a shell command which ungzips
;;;	its standard input to its standard output.
;;;
;;; dired-tar-bzip2-command - a shell command which bzips its
;;;     standard input to its standard output.
;;;
;;; dired-tar-unbzip2-command - a shell command which unbzips
;;;	its standard input to its standard output.
;;;
;;; dired-tar-shell-file-name - name of the shell to use to run the
;;;      tar command.  The default is `shell-file-name'.

;;; Changes since 1.8:
;;; - From Steve Youngs <steve@sxemacs.org>:
;;;   - Add support for lzma and xz tarfiles
;;; Changes since 1.7:
;;; - From Steve Youngs <steve@sxemacs.org>:
;;;   - Add support for bzip2 tarfiles
;;;   - Convert user variables to defcustom's
;;;   - Change key binding to `M-t' (`T' is `dired-do-total-size')
;;; Changes since 1.6:
;;; - recognize files with extension .tgz as gzipped tarfiles; let user
;;;   configure what we name compressed tar files we create.
;;; Changes since 1.5:
;;; - (dired-tar-pack): Changes from Cord Kielhorn: name files correctly
;;;   when dired-tar-should-gzip is false.
;;;
;;; Changes since 1.4:
;;; - added dired-tar-shell-file-name and dired-tar-command-switches;
;;;   thanks to Cristian Ionescu-Idbohrn <cii@kcs.se>!

;;; Code:

(require 'compile)
(eval-when-compile (load "cl-macs"))


;;;; Variables.

(defgroup dired-tar nil
  "Extensions to Dired for handling tarfiles."
  :prefix "dired-tar-"
  :group 'dired)

(defcustom dired-tar-compress-with 'gzip
  "*Compression program to use when creating tarfiles.

Can be the symbols `gzip', `bzip2', `lzma', `xz' for those respective
compression programs, or nil for no compression."
  :type '(choice
	  (item :tag "Use Gzip" gzip)
	  (item :tag "Use Bzip2" bzip2)
	  (item :tag "Use lzma" lzma)
	  (item :tag "Use xz" xz)
	  (item :tag "No compression" nil))
  :group 'dired-tar)

(defcustom dired-tar-gzip-extension ".tar.gz"
  "*File name extension to use for creating gzipped tar files.

By default, this is \".tar.gz\", but some people may like to use
\".tgz\".

NOTE: this variable is only for _creating_ gzipped tarfiles, it isn't
used for unpacking existing tarfiles."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-bzip2-extension ".tar.bz2"
  "*File name extension to use for bzip2'd tar files.

By default, this is \".tar.bz2\", but some people may like to use
\".tbz\" or \".tbz2\".

NOTE: this variable is only for _creating_ bzipped tarfiles, it isn't
used for unpacking existing tarfiles."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-lzma-extension ".tar.lzma"
  "*File name extension to use for lzma'd tar files.

By default, this is \".tar.lzma\", but some people may like to use
\".tlz\".

NOTE: this variable is only for _creating_ lzma compressed tarfiles,
it isn't used for unpacking existing tarfiles."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-xz-extension ".tar.xz"
  "*File name extension to use for xz'd tar files.

By default, this is \".tar.xz\", but some people may like to use
\".txz\".

NOTE: this variable is only for _creating_ xz compressed tarfiles, it
isn't used for unpacking existing tarfiles."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-gzip-command "gzip --best --stdout"
  "*A shell command which gzips its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-bzip2-command "bzip2 --best --stdout"
  "*A shell command which bzip2's its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

;; It's not such a crash-hot idea to use --best with lzma or xz, they
;; do a good enough job without it anyway.
(defcustom dired-tar-lzma-command "lzma --stdout"
  "*A shell command which lzma's its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-xz-command "xz --stdout"
  "*A shell command which xz's its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-ungzip-command "gzip --decompress --stdout"
  "*A shell command which ungzips its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-unbzip2-command "bzip2 --decompress --stdout"
  "*A shell command which unbzip2's its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-unlzma-command "lzma --decompress --stdout"
  "*A shell command which unlzma2's its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-unxz-command "xz --decompress --stdout"
  "*A shell command which unxz's its stdin to its stdout."
  :type 'string
  :group 'dired-tar)

(defcustom dired-tar-shell-file-name shell-file-name
  "*The name of the shell to use to run the tar command."
  :type '(file :must-match t)
  :group 'dired-tar)

(defcustom dired-tar-command-switches "vf"
  "Flags to pass to the tar program, in addition to the command charcaters.

This is concatenated with command characters (\"x\" or \"c\" or
whatever).  The default is 'vf'; I'm told Windows users should use
\"mvf\"."
  :type 'string
  :group 'dired-tar)

(defvar dired-tar-result nil
  "For internal use by dired-tar functions.
This variable is made local to the buffer in which we run the tar
process, and holds the name of the file created or affected.  The
process-termination sentinal uses this to update the dired listing
when the process completes its work, or dies.")


;;;; Internal functions.

(defun dired-tar-run-command (command directory result)
  "Internal function for use by the dired-tar package.
Run COMMAND asynchronously in its own window, like a compilation.
Use DIRECTORY as the default directory for the command's execution.
RESULT is the name of the tar file which will be created, or the
name of the directory into which the tar file was unpacked."
  (let ((buf (dired-tar-get-buffer)))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (widen)
      (erase-buffer)
      (goto-char (point-min))
      (insert "cd " directory)
      (newline)
      (insert command)
      (newline)

      (setq buffer-read-only t
	    mode-name "Tar-Output"
	    default-directory directory)

      (set (make-local-variable 'dired-tar-result)
	   result)
      (set (make-local-variable 'mode-line-process)
	   '(": %s"))
      (set (make-local-variable 'compilation-finish-function)
	   'dired-tar-operation-done)

      (let ((process
	     ;; Chris Moore <Chris.Moore@src.bae.co.uk> says that the
	     ;; tar commands barf using his version of the zsh.  We
	     ;; don't need anything but the Bourne shell here; that's
	     ;; the default value for dired-tar-shell-file-name.
	     (let ((shell-file-name dired-tar-shell-file-name))
	       (start-process-shell-command "*Tar*" buf command))))
	(set-process-sentinel process 'compilation-sentinel))
      (display-buffer buf))))

(defun dired-tar-get-buffer ()
  "Choose a buffer to run a tar process in.
Tar output buffers have names like *Tar*, *Tar*<2>, *Tar*<3>, ...
We return the lowest-numbered buffer that doesn't have a live tar
process in it.  We delete any other buffers whose processes have
deleted."

  ;; Kill all completed tar buffers.
  (let ((number 1))
    (while number
      (let* ((name (if (<= number 1) "*Tar*"
		     (format "*Tar*<%d>" number)))
	     (buf (get-buffer name)))
	(if (null buf) (setq number nil)
	  (save-excursion
	    (set-buffer buf)
	    (if (let ((process (get-buffer-process buf)))
		  (not (and process (eq (process-status process) 'run))))
		(kill-buffer buf)))
	  (setq number (1+ number))))))

  ;; Make us a fresh buffer.
  (generate-new-buffer "*Tar*"))
	

(defun dired-tar-operation-done (buf message)
  "Internal function for use by the dired-tar package.
This function is run when the tar operation completes.  It tries to
update the dired listing by looking at dired-tar-result."
  (cond
   ((null dired-tar-result))

   ((file-directory-p dired-tar-result)
    (save-excursion
      (mapcar
       (function (lambda (buf)
		   (set-buffer buf)
		   (dired-revert)))
       (dired-buffers-for-dir dired-tar-result))))

   ((file-exists-p dired-tar-result)
    (dired-relist-file dired-tar-result))

   ;; Otherwise, I guess the tar operation must have failed somehow.
   ))

(defun dired-tar-pack (directory prefix-arg)
  "Internal function for use by the dired-tar package.

Create a tar file from the contents of DIRECTORY, compressed with
`dired-tar-compress-with'.  The archive is named after the directory,
and the files are stored in the archive with names relative to
DIRECTORY's parent.

If `dired-tar-compress-with' is nil, the tar file will not be compressed.

We use `dired-tar-gzip-extension', `dired-tar-bzip2-extension',
`dired-tar-lzma-extension', or `dired-tar-xz-extension' as the
suffix for the filenames we create.  Or just \".tar\" if the tar
file is not compressed.

For example, (dired-tar-pack \"/home/blandy/womble/\") could produce a
tar file named \"/home/blandy/womble.tar.gz\", whose contents had
names like \"womble/foo\", \"womble/bar\", etcetera.

The second argument PREFIX-ARG is ignored."
  (let* ((dir-file (directory-file-name directory))
	 (tar-file-name
	  (case dired-tar-compress-with
	    (gzip (concat dir-file dired-tar-gzip-extension))
	    (bzip2 (concat dir-file dired-tar-bzip2-extension))
	    (lzma (concat dir-file dired-tar-lzma-extension))
	    (xz (concat dir-file dired-tar-xz-extension))
	    (otherwise (format "%s.tar" dir-file))))
	 (parent-name (file-name-directory dir-file))
	 (content-name (file-name-nondirectory dir-file)))
    (dired-tar-run-command
     (case dired-tar-compress-with
       (gzip (format "tar cvf - %s | %s > %s"
		     content-name
		     dired-tar-gzip-command
		     tar-file-name))
       (bzip2 (format "tar cvf - %s | %s > %s"
		      content-name
		      dired-tar-bzip2-command
		      tar-file-name))
       (lzma (format "tar cvf - %s | %s > %s"
		      content-name
		      dired-tar-lzma-command
		      tar-file-name))
       (xz (format "tar cvf - %s | %s > %s"
		      content-name
		      dired-tar-xz-command
		      tar-file-name))
       (otherwise (format "tar cvf %s %s"
			  tar-file-name
			  content-name)))
     parent-name
     tar-file-name)))

(defconst dired-tar-tarfile-regexp
  (format "\\(%s\\)\\'"
	  (mapconcat 'regexp-quote
		     '(".tar" ".tar.z" ".tar.gz" ".tar.Z" ".tgz" ".tar.bz2"
		       ".tbz" ".tbz2" ".tar.lzma" ".tlz" ".tar.xz" ".txz" )
		     "\\|"))
  "Regular expression matching plausible filenames for tar files.")

(defconst dired-tar-gzipped-tarfile-regexp
  (format "\\(%s\\)\\'"
	  (mapconcat 'regexp-quote
		     '(".tar.z" ".tar.gz" ".tar.Z" ".tgz")
		     "\\|"))
  "Regular expression matching plausible filenames for gzip compressed tar files.")

(defconst dired-tar-bzipped-tarfile-regexp
  (format "\\(%s\\)\\'"
	  (mapconcat 'regexp-quote
		     '(".tar.bz2" ".tbz" ".tbz2")
		     "\\|"))
  "Regular expression matching plausible filenames for bzip2 compressed tar files.")

(defconst dired-tar-lzma-tarfile-regexp
  (format "\\(%s\\)\\'"
	  (mapconcat 'regexp-quote
		     '(".tar.lzma" ".tlz")
		     "\\|"))
  "Regular expression matching plausible filenames for lzma compressed tar files.")

(defconst dired-tar-xz-tarfile-regexp
  (format "\\(%s\\)\\'"
	  (mapconcat 'regexp-quote
		     '(".tar.xz" ".txz")
		     "\\|"))
  "Regular expression matching plausible filenames for xz compressed tar files.")

(defun dired-tar-unpack (tar-file prefix-arg)
  "Internal function for use by the dired-tar package.
Unpack TAR-FILE into the directory containing it.
If PREFIX-ARG is non-nil, just list the archive's contents without
unpacking it."

  (let ((tar-file-dir (file-name-directory tar-file))
	(action (if prefix-arg "t" "x")))
    (dired-tar-run-command
     (cond

      ;; Does this look like a tar file at all?
      ((not (string-match dired-tar-tarfile-regexp tar-file))
       (error
	"bug: dired-tar-unpack should only be passed tar file names."))

      ;; Does it look like a compressed tar file?
      ((string-match dired-tar-gzipped-tarfile-regexp tar-file)
       (format "%s < %s | tar %s%s -"
	       dired-tar-ungzip-command
	       tar-file
	       action
	       dired-tar-command-switches))

      ;; Does it look like a bzip2 compressed tar file?
      ((string-match dired-tar-bzipped-tarfile-regexp tar-file)
       (format "%s < %s | tar %s%s -"
	       dired-tar-unbzip2-command
	       tar-file
	       action
	       dired-tar-command-switches))

      ;; Does it look like a lzma compressed tar file?
      ((string-match dired-tar-lzma-tarfile-regexp tar-file)
       (format "%s < %s | tar %s%s -"
	       dired-tar-unlzma-command
	       tar-file
	       action
	       dired-tar-command-switches))

      ;; Does it look like a xz compressed tar file?
      ((string-match dired-tar-xz-tarfile-regexp tar-file)
       (format "%s < %s | tar %s%s -"
	       dired-tar-unxz-command
	       tar-file
	       action
	       dired-tar-command-switches))

      ;; Okay, then it must look like an uncompressed tar file.
      (t
       (format "tar %svf %s" action tar-file)))
     tar-file-dir

     ;; If we're just unpacking the archive, don't bother updating the
     ;; dired listing.
     (if prefix-arg nil tar-file-dir))))


;;;; User-visible functions.

;;;###autoload
(defun dired-tar-pack-unpack (prefix-arg)
  "Create or unpack a tar archive for the file on the current line.

If the file on the current line is a directory, make a gzipped tar
file out of its contents.

If the file on the current line is a tar archive, unpack it.  If the
archive appears to be gzipped or compressed, expand it first.  With a
prefix argument, just list the tar archive's contents, and don't unpack
it.  The file's name must end in \".tar\", \".tar.gz\", \".tar.Z\",
\".tar.bz2\", \".tbz\", \".tbz2\", \".tar.lzma\", \".tlz\", \".tar.xz\",
or \".txz\" otherwise this command will assume it's not a tar file."
  (interactive "P")

  (let ((filename (dired-get-filename)))
    (cond
     ((file-directory-p filename)
      (dired-tar-pack filename prefix-arg))

     ((string-match dired-tar-tarfile-regexp filename)
      (dired-tar-unpack filename prefix-arg))

     (t
      (error "%s is neither a tar file nor a directory" filename)))))


;;;; Hooking this into dired mode.

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map [(meta ?t)] 'dired-tar-pack-unpack)))


(provide 'dired-tar)

;;; dired-tar.el ends here
