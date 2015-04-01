;; cliaspora.el --- Simple Emacs-based client for cliaspora*

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

;; Posting

(require 'cl)
(require 'markdown-mode)
(require 'cliaspora-post-edit-mode)

(defcustom cliaspora-header-post
  "### "
  "Header for each post:"
  :type 'string
  :group 'cliaspora)

(defcustom cliaspora-footer-post
  "#cliaspora-el"
  "Footer for each post."
  :type 'string
  :group 'cliaspora)

(defcustom cliaspora-save-after-posting t
  "*Non-nil means automatically save after posting."
  :type 'boolean
  :group 'cliaspora)

(defun cliaspora-post-to (&optional initial)
  "Post to cliaspora.
With a prefix, uses the region as INITIAL.
For example: C-u M-x cliaspora-post-to."
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (window-configuration-to-register cliaspora-post-register)
  (get-buffer-create cliaspora-post-buffer)  
  (switch-to-buffer-other-window cliaspora-post-buffer)
  (with-current-buffer cliaspora-post-buffer
    (let ((inhibit-read-only t))
      (cliaspora-date)
      (insert cliaspora-footer-post)
      (goto-char (point-min))
      (when initial 
        (insert initial))
      (goto-char (point-min))
      (insert cliaspora-header-post)
      (cliaspora-mode)
      (cliaspora-set-send-type 'post)
      (cliaspora-post-edit-mode)      
      (set 'buffer-read-only nil)
      )
    )
  (message "Use C-cp to post to cliaspora*."))

(defun cliaspora-add-aspect (aspect-name)
  "Add an aspect to the list of aspects `cliaspora-aspects-for-post' for posting.
This list is used as parameter for `cliaspora-post'."
  (interactive 
   (let ((string (progn 
                   (cliaspora-get-aspects)
                   (completing-read "Aspect name?  " cliaspora-aspect-alist)))
         )
     (list string)))
  (let ((aspect-id (cdr (assoc aspect-name cliaspora-aspect-alist))))
    (if (null aspect-id)
        (message "Aspect not founded.")
      (progn 
        (setq cliaspora-aspects-for-post (push aspect-name cliaspora-aspects-for-post))
        (message (concat "Aspect id Added: " 
                         (if (numberp aspect-id)
                             (number-to-string aspect-id)
                           aspect-id)))))))

(defun cliaspora-clear-selected-aspects ()
  "Clear all the selected aspect to use with the next post."
  (interactive)
  (setq cliaspora-aspects-for-post nil))

(defun cliaspora-selected-aspects ()
  "Show the selected aspects to use with the newly post."
  (interactive)
  (let ((msg "Aspects: \n"))
    (dolist (i cliaspora-aspects-for-post)
      (setq msg (concat msg 
                        "-> "
                        (car (rassoc i cliaspora-aspect-alist)) 
                        "\n")))
    (message msg)))

(defun cliaspora-authenticity-token (url)
  "Get the authenticity token."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
                    (list (cons "user[username]" cliaspora-username)
                          (cons "user[password]" cliaspora-password)
                          (cons "user[remember_me]" "1")
;			  (cons "utf8" "✓")
                          )
                    "&")))
;;    (debug)
    (with-current-buffer (url-retrieve-synchronously url)
      (cliaspora-find-auth-token))))

(defun cliaspora-find-auth-token (&optional status)
  "Find the authenticity token."  
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp "<meta name=\"csrf-token\" content=\"\\(.*\\)\"/>" nil t)
      (search-forward-regexp "<meta content=\"\\(.*\\)\" name=\"csrf-token\"[[:blank:]]*/>" nil t))
    (setq cliaspora-auth-token (match-string-no-properties 1)))
  cliaspora-auth-token)

(defun cliaspora-aspect-post-parameter (aspects_ids)
  "Concat the parameters in a string with commas. This is usefull to pass
as parameters for a POST.

If aspects_ids is nil, I return the string \"public\".

It doesn't matter if aspects_id has a string or number values as elements(or mixed!) it will concat it as well. "
  (if (null aspects_ids)
      "public"    
    (let ((salida ""))
      (dolist (i aspects_ids)
        (setq salida (concat salida 
                             (if (numberp i)
                                 (number-to-string i)
                               i)
                             ",")))  
      (substring salida 0 -1))))

(defun cliaspora-post-last-post-text ()
  (interactive)
  (cliaspora-post cliaspora-last-post-text))

(defun cliaspora-image-list (lst-photos-ids)
  "Return a list with cons where the first element is the string \"photos[n]\" changing n accordingly, and the second element is the photo id taken from LST-PHOTOS-IDS."
  (let ((num 0)
        (outlst nil)
        )
    (dolist (e lst-photos-ids)
      (push (cons "photos[]" (number-to-string e)) outlst)
      (setq num (+ 1 num))
      )
    outlst
    )
  )

(defun cliaspora-post (post &optional aspects_ids photos-ids)
  "Post POST to cliaspora.
ASPECTS_IDS is a list of strings or numbers of aspects ids.
PHOTOS-IDS is a list of strings or numbers of photos ids."
  (let ((final-aspect (if aspects_ids (car  aspects_ids) "public")))
        (with-temp-file "/tmp/outfile.md"
          (insert post))
        (shell-command-to-string (concat "cat /tmp/outfile.md | cliaspora post " final-aspect "; rm /tmp/outfile.md"))))

(defun cliaspora-post-this-buffer ()
  "Post the current buffer to cliaspora."
  (interactive)
  (cliaspora-post (buffer-string) cliaspora-aspects-for-post cliaspora-images-posted)
  (setq cliaspora-images-posted nil)
  (cliaspora-save-post-to-file)
  ;;(kill-buffer)
  )

(defsubst cliaspora-date ()
  "Date string for inserting in posts."
  (interactive)
  (insert "\n\n#" (format-time-string "%Y%m%d") " "))

  
(defun cliaspora-save-post-to-file ()
  "Save post to backup file. Backup file is ymd, a new post is append."
  (with-temp-buffer
    (insert-buffer cliaspora-post-buffer)
    (setq cliaspora-last-post-text (buffer-string)) ;this is temporary...
    (insert "\n" "---" "\n")
    (let ((file-name-for-saving-post (format-time-string "%y%m%d")))
      (if (find-buffer-visiting file-name-for-saving-post)
          (let ((post-text (buffer-string)))
            (set-buffer (get-file-buffer (concat cliaspora-posts-directory file-name-for-saving-post)))
            (save-excursion
              (goto-char (point-max))
              (insert post-text)
              (insert "\n")
              (when cliaspora-save-after-posting (save-buffer))))
        (append-to-file (point-min) (point-max) 
                        (concat cliaspora-posts-directory file-name-for-saving-post))))))

(defun cliaspora-find-all-regexp (regexp &optional num)
  "Try to find all the texts with match the given regexp.
Return all of them in a list.
NUM is the parenthesized expression in the regexp, like parameter NUM in `match-string-no-properties'."
  (cond ((search-forward-regexp  regexp (point-max) t)
         (cons (match-string-no-properties (if (not num) 0 num))
               (search-forward-regexp regexp))
         )	 
	
        (t 
         nil)
        )
  )

	
(defun cliaspora-find-all-markdown (regexp &optional opt)
  "Find all markdown strings given by REGEXP and return all of them in a list.
Usage example: `(cliaspora-find-all-markdown cliaspora-regex-tag)'"
  (remove-duplicates (cliaspora-find-all-regexp regexp opt) :test 'equal)
  )


(defun cliaspora-post-buffer-desc ()
  "Using the first line of the current buffer."
  (interactive)
  (let ((post (buffer-substring (point-min)
                                (save-excursion
                                  (goto-char (point-min))
                                  (end-of-line)
                                  (if (> (- (point) (point-min)) 60)
                                      (goto-char (+ (point-min) 60)))
                                  (point)))))
    (cliaspora-ask)
    (cliaspora-post post)))

(defun cliaspora-post-clipboard ()
  "Post to cliaspora the contents of the current clipboard.
Most useful for posting things from any where."
  (interactive)
  (cliaspora-ask)
  (cliaspora-post-to (current-kill 0)))

(defun cliaspora-post-destroy ()
  "Destroy the current cliaspora post buffer."
  (interactive)
  (when (equal cliaspora-post-buffer (buffer-name))
    (kill-buffer (current-buffer))
    (jump-to-register cliaspora-post-register)))


(defun cliaspora-short-url (url)
  "Short URL function, uses is.gd."
  (interactive "s")
  (let ((url-request-method "GET"))
    (url-retrieve (concat "http://is.gd/create.php?format=simple&url=" url)
                  (lambda (x)
                    (goto-char (point-min))
                    (search-forward-regexp "http://.*")
                    (setq s-url (match-string-no-properties 0))))
    (insert s-url)))

(defun cliaspora-post-send-image (image-path url)
  "Send an image file given by IMAGE-PATH to the given URL."
  (with-current-buffer (find-file-literally image-path)
    (let ((image-data nil)
          (url-request-method "POST")
          (url-request-extra-headers
           (list (cons "Content-Type" "application/octet-stream")
                 (cons "X-File-Name" (file-name-nondirectory image-path))
                 (cons "X-CSRF-Token" cliaspora-auth-token)
                 (cons "Referer" "https://joincliaspora.com/stream")
                 )	   
           )
          (url-request-data (buffer-string))	  
          )
      (with-current-buffer (url-retrieve-synchronously url)
        (cliaspora-delete-http-header)
        (goto-char (point-min))
        (setq image-data (json-read));; save image url and data for history
        )      
      (kill-buffer (current-buffer))      
      (cliaspora-save-image-data image-data)
      (push (cdr (assoc 'id (cliaspora-image-data-get-photo-data image-data)))
            cliaspora-images-posted)

      )
    )
  )

(defvar cliaspora-images-posted
  nil
  "This is a list of images ids that has been posted.

This variable should store all the images ids of those unpublished images(temporary submitted).

It will be erased when you use `cliaspora-post-this-buffer' or simmilar functions that post the message with the images ids."
  )

(defun cliaspora-save-image-data (image-data)
  "Save the image data in a history file `cliaspora-image-history-file'."
  (let* ((photo-data (cliaspora-image-data-get-photo-data image-data))
         (photo-urls (assoc 'unprocessed_image photo-data))
         )
    (with-temp-buffer 
      (if (file-exists-p cliaspora-image-history-file)
          (insert-file-contents cliaspora-image-history-file)
        )
      (insert (format "\n* Photo
%s
** URL
%s
** Scaled full:
%s"
                      (cdr (assoc 'id photo-data))	      
                      (cdr (assoc 'url photo-urls))		      
                      (cdr (assoc 'url (cdr (assoc 'scaled_full photo-urls))))
                      )
              )
      (write-file cliaspora-image-history-file nil)
      )
    )
  )

(defun cliaspora-image-data-get-photo-data (image-data)
  (cdr (assoc 'photo (cdr (assoc 'data image-data))))
  )

(defun cliaspora-add-image (image-path)
  "Add an image to the next post."
  (interactive "fImage file?")
  (cliaspora-ask)
  (cliaspora-get-authenticity-token-if-necessary)
  (unless cliaspora-aspects-for-post
    (cliaspora-get-aspects)
    ;; (setq cliaspora-aspects-for-post (cliaspora-get-values cliaspora-aspect-alist))
    ;; (setq cliaspora-aspects-for-post (remove "all_aspects"
    ;; 					    (remove "public" cliaspora-aspects-for-post)))
    (setq cliaspora-aspects-for-post '("public"))
    )
  (cliaspora-post-send-image image-path 
                             (cliaspora-image-url t
                                                  cliaspora-aspects-for-post 
                                                  (file-name-nondirectory image-path)
                                                  )
                             )
  )
  

(provide 'cliaspora-post)
