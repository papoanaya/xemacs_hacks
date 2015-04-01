;;; cliaspora-comments.el --- 
;; 
;; Filename: cliaspora-comments.el
;; Description: 
;; Author: Christian Giménez, Tiago Charters Azevedo
;; Maintainer: 
;; Created: mar feb 14 13:15:51 2012 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Functions for manage comments for diáspora.el.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cliaspora-post-edit-mode)

					; ********************
					; Customization

(defcustom cliaspora-comment-name 
  "comments"
  "This is the name of the comments for posting."
  :type 'string
  :group 'cliaspora-streams)


					; ********************
					; Functions

(defun cliaspora-insert-comments-for-message (message-id &optional buffer)
  "Get the comments for the given message, and insert it in the current 
buffer or in the buffer specified."
  (cliaspora-ask)
  (cliaspora-get-authenticity-token-if-necessary)
  (let ((buff-http (cliaspora-get-url-entry-stream 
		    (cliaspora-get-comment-url message-id)))
	(buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buff-http
      (cliaspora-delete-http-header)
      (let ((json-array-type 'list)
	    (json-object-type 'alist)
	    (lstparsed (json-read)))
	;; parse all comments one by one and insert it
	(let ((le (length lstparsed))
;;	      (inhibit-read-only t)
	      )
	  (dotimes (i le)
	    (cliaspora-insert-comment (aref lstparsed i) buffer)))))))
	    
(defun cliaspora-insert-comment (comment buffer)
  "Insert a JSON parsed (with `json-read') into a specific buffer."
  (let ((name (cdr (assoc 'name (cdr (assoc 'author comment)))))
	(avatar (cdr (assoc 'large (cdr (assoc 'avatar (cdr (assoc 'author comment)))))))
	(cliaspora-id (cdr (assoc 'cliaspora_id (cdr (assoc 'author comment)))))
	(text (cdr (assoc 'text comment)))
	(created_at (cdr (assoc 'created_at comment))))
    (with-current-buffer buffer
      (insert (format "\n![%s](%s)" name avatar))
      (insert "\n"
	      (propertize
	       (format "%s (%s): at %s:" name cliaspora-id created_at)
	       'cliaspora-is-comment-user-name t
	       )
	      "\n"
	      )
      (insert (propertize
	       text
	       'cliaspora-is-comment-text t)
	      "\n")
      
      ))
  )


(defconst cliaspora-comment-buffer-name "*cliaspora comment*"
  "This is the name of the comment buffer.")

(defvar cliaspora-comment-buffer nil
  "This is the buffer (supposed to be only one or unique) for write a comment.")

(defun cliaspora-comment-message ()
  "Find the post-id and create a buffer for the user so he can write a comment.

This function set the `cliaspora-next-comment-to-post' variable with the post-id."
  (interactive)
  ;;(Set 'cliaspora-next-comment-to-post (cliaspora-get-id-message-near-point))
  (cond ((search-backward-regexp "POST-ID: \\([0-9]+\\)" nil t) 
         (cliaspora-new-comment-buffer (match-string 1)))
         ((search-forward-regexp "POST-ID: \\([0-9]+\\)" nil t)   
          (cliaspora-new-comment-buffer (match-string 1)))
         (t (message "Unable to find the post-id (located in the text property `cliaspora-id-message')."))))



(defun cliaspora-new-comment-buffer (&optional post-id)
  "Create a new buffer for write a comment for the post with id given by post-id.

If post-id parameter is not given, use the message id from `cliaspora-next-comment-to-post'."

  (when post-id
    (set 'cliaspora-next-comment-to-post post-id))
  ;; create buffer
  (set 'cliaspora-comment-buffer (get-buffer-create cliaspora-comment-buffer-name))
  (switch-to-buffer-other-window cliaspora-comment-buffer)
  (with-current-buffer cliaspora-comment-buffer
    (let ((inhibit-read-only t))
      ;; insert header and footer, modes... etc.
      (cliaspora-date)
      (insert cliaspora-footer-post)
      (goto-char (point-min))
      (insert cliaspora-header-post)
      (cliaspora-mode)
      (cliaspora-set-send-type 'comment)
      (cliaspora-post-edit-mode)      
      (set 'buffer-read-only nil)
    ))
  (message "Use C-c C-c to comment to cliaspora or use cliaspora-send-comment-this-buffer."))

(defvar cliaspora-next-comment-to-post
  nil
  "This is the post id where to send the comment in the next `cliaspora-send-comment-this-buffer' function call.")

(defun cliaspora-send-comment-this-buffer (&optional new-auth-token)
  "Send this buffer as a comment to the post determined by the id `cliaspora-next-comment-to-post'.

If new-auth-token is set to t, its get a new authenticity token. This is usefull when sometimes the comment doesn't work."
  (interactive "P")
  (cliaspora-send-comment-post (buffer-string) cliaspora-next-comment-to-post))


(defun cliaspora-send-comment-post (comment post-id)
  "Send a comment for the post given by the post-id.
Comment should be a String and post-id the id number of the post."
  (progn
    (with-temp-file "/tmp/outfile.md"
      (insert comment))
    (shell-command-to-string (concat "cat /tmp/outfile.md | cliaspora comment " post-id "; rm /tmp/outfile.md"))))

;; Add keymap for `cliaspora-mode':



(defun cliaspora-comments-show-last-three (message-json-parsed)
  "Insert in the current buffer the last three comments part of a JSON parsed message taken from a stream.

This function is usefull when you are showing a stream with lots of posts."
  (let* ((lst-msgs (cdr (assoc 'last_three_comments message-json-parsed)))
	 (amount-comments (length lst-msgs))
	 )
    (if (arrayp lst-msgs)
	(dotimes (i amount-comments) ;;lst-msgs is an array!
	  (cliaspora-insert-comment (aref lst-msgs i) (current-buffer))
	  )
      (dolist (elt lst-msgs)
	(cliaspora-insert-comment elt (current-buffer))
	)
      )
    )  
  )

(provide 'cliaspora-comments)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-comments.el ends here
