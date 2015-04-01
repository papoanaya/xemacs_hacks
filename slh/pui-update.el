;; sy-pui-update.el --- Update packages from cron.

;;; Commentary:
;;    This is hardly worth putting a copyright notice on, so you can
;;    do whatever you like with this. :-)
;;    
;;    Set the download mirror and directory to your liking.  It's hard
;;    coded because we don't want to waste our time loading up any
;;    customisations.  Put it in you load-path and then add something
;;    like this to your crontab:
;;
;;    15 3 * * sun xemacs -batch -vanilla -l pui-update -f pui-update-all
;;
;;    Then every Sunday at 3:15am your installed packages will be
;;    updated.

;;; Code:

(require 'package-get)
(require 'ffi-curl)

(defvar pui-update-mirror '("ftp.au.xemacs.org" "pub/xemacs/beta/experimental/packages")
  "Mirror to use")

(defun pui-update-fetch-index ()
  (let ((remote (concat "ftp://"
			(nth 0 pui-update-mirror)
			"/"
			(nth 1 pui-update-mirror)
			"/package-index.LATEST.gpg"))
	(local (expand-file-name "package-index.LATEST.gpg"
				 user-init-directory)))
    (curl:download remote local)))

(defun pui-update-all ()
  (interactive)
  (let ((package-get-remote pui-update-mirror)
	(efs-use-passive-mode t))
    (epa-file-disable)
    (package-get-update-all)))

;     (pui-update-fetch-index)
;     (catch 'exit
;       (mapcar (lambda (pkg)
; 		(if (not (package-get (car pkg) nil 'never))
; 		    (throw 'exit nil))) ;; Bail out if error detected
; 	      packages-package-list))))

(provide 'pui-update)
;;; sy-pui-update.el ends here
