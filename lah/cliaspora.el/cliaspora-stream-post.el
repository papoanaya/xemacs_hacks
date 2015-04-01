;;; cliaspora-edit-mode.el --- 
;; 
;; Filename: cliaspora-edit-mode.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr 25 11:23:06 2012 (-0300)
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
;; 
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


(defvar cliaspora-send-type nil
  "This buffer shows what type of message is in the current buffer.
You should make it as a buffer-local variable using `make-local-variable' function and use it accordingly.

Expected values are:
* nil      :: It is not determined or unknown.
* 'post    :: Is a new post. 
* 'comment :: Is a new comment."
  )

(defun cliaspora-set-send-type (type &optional buffer)
  "Set the type of buffer: 

* If it is a new post set TYPE to 'post.
* If it is a new comment set TYPE to 'comment"
  (unless buffer
    (setq buffer (current-buffer))
    )
  (with-current-buffer buffer
    (set (make-local-variable 'cliaspora-send-type) type)
    )
  )

(defvar cliaspora-post-edit-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'cliaspora-send-post-or-comment-this-buffer)
    (define-key map "\C-c\C-h4" 'cliaspora-markdown-insert-headline-4)
    (define-key map "\C-c\C-h3" 'cliaspora-markdown-insert-headline-3)
    (define-key map "\C-c\C-h2" 'cliaspora-markdown-insert-headline-2)
    (define-key map "\C-c\C-h1" 'cliaspora-markdown-insert-headline-1)
    (define-key map "\C-c\C-u" 'cliaspora-markdown-insert-unordered-list)
    (define-key map "\C-c\C-e" 'cliaspora-markdown-insert-emph-text)
    (define-key map "\C-c\C-b" 'cliaspora-markdown-insert-bold-text)
    (define-key map "\C-c\C-r" 'cliaspora-markdown-insert-horizontal-rule)
    (define-key map "\C-c\C-l" 'cliaspora-markdown-insert-link)
    (define-key map "\C-c\C-i" 'cliaspora-markdown-insert-image)
    (define-key map "\C-c\C-m" 'cliaspora-markdown-mention-user)
    (define-key map "\C-c\C-t" 'cliaspora-markdown-insert-tag)
    map
    )
  "Keymap for `cliaspora-post-edit-mode'."
  )

(defvar cliaspora-post-edit-mode-keywords
  '(t (
       (cliaspora-regexp-tag . cliaspora-tag-face)
       (cliaspora-regexp-user-name . cliaspora-user-name-citation-face)
       )
      )
  "Keywords for `cliaspora-post-edit-mode' minor mode."
  )
  

(define-minor-mode cliaspora-post-edit-mode 
  "Minor mode for adding keymaps and highlightings according to D*."
  nil
  " D*-post-edit"
  cliaspora-post-edit-mode-map
  :group 'cliaspora

  ;; (if cliaspora-post-edit-mode
  ;;     (cliaspora-pem-add-keywords)
  ;;   (cliaspora-pem-remove-keywords)
  ;;   )	    
  
  (unless cliaspora-send-type
    (cliaspora-set-send-type 'post)
    (message "Use C-c C-c to send this buffer as a new POST")
    )
  )

;; "pem" = "post edit mode". As abreviation we use "pem" instead of "post-edit-mode".

(defun cliaspora-pem-add-keywords ()
  "Append the `cliaspora-post-edit-mode-keywords' into the `font-lock-defaults'."
  (setq font-lock-keywords 
	(append cliaspora-post-edit-mode-keywords font-lock-keywords)
	)
  )

(defun cliaspora-pem-remove-keywords ()
  "Remove the `cliaspora-post-edit-mode-keywords' from the `font-lock-defaults'."
  (dolist (e cliaspora-post-edit-mode-keywords)
    (setq font-lock-defaults (remove e font-lock-defaults))
    )
  )

(defun cliaspora-send-post-or-comment-this-buffer (&rest r)
  "Depending if this is a comment or post, send it.
Use `cliaspora-post-this-buffer' for posting, or `cliaspora-send-comment-this-buffer' if it is a new comment.

I read the `cliaspora-send-type' variable and reset it to nil after sending."
  (interactive)
  (cond
   ((equal cliaspora-send-type 'post)
    (cliaspora-post-this-buffer)
    (message "Buffer POSTED!")
    )
   ((equal cliaspora-send-type 'comment)
    (cliaspora-send-comment-this-buffer)
    (message "COMMENT sended!")
    )
   (t 
    (message "D* cannot determine the type of buffer you are trying to send. Use C-cp for sending a new post or C-cc for sending a new comment."))
   )
  (set (make-local-variable 'cliaspora-send-type) nil)
  )

(define-skeleton cliaspora-markdown-insert-headline-2
  "Headline 2."
  "Text: "
  "## " str \n \n)

(define-skeleton cliaspora-markdown-insert-headline-3
  "Headline 3."
  "Text: "
  "### " str \n \n)

(define-skeleton cliaspora-markdown-insert-headline-4
  "Headline 4."
  "Text: "
  "#### " str \n \n)

(define-skeleton cliaspora-markdown-insert-unordered-list
  "Unordered list."
  "Text: "
  "* " str \n \n)

(define-skeleton cliaspora-markdown-insert-emph-text
  "Emphasis."
  "Text: "
  "*" str "*")

(define-skeleton cliaspora-markdown-insert-bold-text
  "Bold."
  "Text: "
  "**" str "**")

(define-skeleton cliaspora-markdown-insert-horizontal-rule
  "Horizontal rule tag."
  nil
  "---" \n \n)

(define-skeleton cliaspora-markdown-insert-link
  "Link"
  "Text: "
  "[" str "](http://" _ ")")

(define-skeleton cliaspora-markdown-insert-image
  "Image with URL."
  "Text: "
  "![" str "](http://" _ ")")

(define-skeleton cliaspora-markdown-mention-user
  "Mention user."
  "User: "
  "@{" str " ; " _ (concat "@" cliaspora-pod "}"))

(define-skeleton cliaspora-markdown-insert-tag
  "Tag."
  "Tag(without #): "
  "#" str )

(provide 'cliaspora-post-edit-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-edit-mode.el ends here
