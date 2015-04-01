;;; cliaspora-messages.el --- 
;; 
;; Filename: cliaspora-messages.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: lun abr 16 23:38:38 2012 (-0300)
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

(require 'cliaspora-urls)
(require 'htmlr)

(defconst cliaspora-messages-buffer-name "*Cliaspora Messages*"
  "This is the buffer-name of the cliaspora messages list.")
(defconst cliaspora-message-buffer-name "*Cliaspora One Message*"
  "This is the buffer-name for showing a cliaspora message."
  )

(defun cliaspora-messages ()
  "List the messages that are in the convesations-url."
  (interactive)
  (let ((buffer-to (get-buffer-create cliaspora-messages-buffer-name))
	(inhibit-read-only t)
	)
    (with-current-buffer buffer-to
      (delete-region (point-min) (point-max))
      (cliaspora-mode)
      (cliaspora-stream-mode)
      (with-current-buffer (cliaspora-messages-get-list)) 
      (goto-char (point-min))
      )
    (switch-to-buffer buffer-to)
    )
  )
  

(defun cliaspora-messages-get-list ()
  "Get all the list of messages from the URL made by `cliaspora-url-json' and `cliaspora-messages-url'. 
Then return a temporary buffer with the messages"
  (let ((buff (shell-command-to-string "cliaspora list messages")))
    buff))



(defun cliaspora-messages-show-list-element (json-elt)
  "Show a list element reading the information from a JSON element part."
  (let ((id (cdr (assoc 'id json-elt)))
	(subject (cdr (assoc 'subject json-elt)))
	(updated (cdr (assoc 'updated_at json-elt)))
	(created (cdr (assoc 'created_at json-elt)))
	(author-id (cdr (assoc 'author_id json-elt)))
	)
    (insert (concat
	     (propertize 
	      "          ====================          \n"
	      'cliaspora-message-separator t)))
    (insert subject "\n")
    (insert (format "%s\nAuthor-id %s" updated author-id) "\n")
    (insert (cliaspora-add-link-to-userstream "Autor Stream" author-id) " | ")
    (insert (cliaspora-add-link-to-message "Read" id) "\n")    
    )
  )

(defvar cliaspora-message-show-msg-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-message-at-point-show)
    (define-key map [mouse-2] 'cliaspora-message-at-point-show)
    map)
  "Keymap used when the user clics on a name link.")

(defun cliaspora-add-link-to-message (text msg-id)
  (propertize 
   text
   'mouse-face 'cliaspora-mouse-highlight-face
;   'face "link"
   'keymap cliaspora-message-show-msg-map
   'cliaspora-conversation-id msg-id ;;We use cliaspora-conversation-id because of misinterpretation
   'cliaspora-is-link-to-pub t
   'help-echo "Click here to see this message in new buffer.")
  )

(defun cliaspora-message-at-point-show (&rest r)
  "Look the message id near point and show it using `cliaspora-message-show'."
  (interactive)
  (cliaspora-message-show (cliaspora-get-conversation-id-near-point))
  )

(defun cliaspora-get-conversation-id-near-point ()
  "Get the cliaspora-conversation-id property value searching from point.
Use it for getting the nearest id post number when selecting a message."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'cliaspora-conversation-id))
		     'cliaspora-conversation-id)
  )



(defun cliaspora-message-show (msg-id)
  "Show a conversation message in a new buffer."
  (cliaspora-ask)
  (cliaspora-get-authenticity-token-if-necessary)
  (let ((buffer-to (get-buffer-create cliaspora-message-buffer-name))
	(text nil)
	)
    (with-current-buffer (cliaspora-get-url (cliaspora-messages-url msg-id))
      (let ((buffer-file-coding-system 'utf-8))
	(cliaspora-delete-http-header)
	(cliaspora-message-delete-unnecessary)
	(cliaspora-message-replace-necessary)
	(setq text (buffer-string))
	)
      )
    (with-current-buffer buffer-to
      (let ((inhibit-read-only t)
	    (buffer-file-coding-system 'utf-8)
	    )
	(delete-region (point-min) (point-max))
	(insert (string-as-multibyte text))
	(goto-char (point-min))
	(htmlr-render)
	(goto-char (point-min))
	(switch-to-buffer buffer-to)
	)
      )    
    )  
  )
  
(defun cliaspora-message-delete-unnecessary ()
  "Remove whatever is unneded for displaying a message."
  (when (search-forward "<h3 class='ltr'>" nil t)
    (delete-region (point-min) (match-beginning 0))
    )  
  (when (search-forward "<textarea cols=" nil t) 
    (delete-region (match-beginning 0) (point-max))
    )
  )

(defun cliaspora-message-replace-necessary ()
  "Replace text so you can see the message in a better way."
  (save-excursion 
    (goto-char (point-min))
    (while (search-forward "<div class='ltr'>" nil t)      
      (goto-char (match-end 0))
      (insert "\n<hr />\n")
      )
    )
  )


(provide 'cliaspora-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-messages.el ends here
