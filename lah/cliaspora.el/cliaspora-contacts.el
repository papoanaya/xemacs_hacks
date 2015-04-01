;;; cliaspora-contacts.el --- 
;; 
;; Filename: cliaspora-contacts.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr 11 10:52:26 2012 (-0300)
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

(require 'json)
(require 'cliaspora-urls)
(require 'cliaspora-mode)
(require 'cliaspora-stream)

(defcustom cliaspora-contact-url
  "contacts"
  "This is the rest of the URL for getting the contact. The first part is usually taken from `cliaspora-pod' variable.

See `cliaspora-url' and `cliaspora-url-json'."
  :type 'string
  :group 'cliaspora-url
  )

(defconst cliaspora-contact-buffer-name "*Cliaspora Contacts*"
  "This is the name of the contact buffer.")

(defun cliaspora-get-all-contacts ()
  "Retrieve the contact list and print it in a buffer called `cliaspora-contact-buffer-name'."
  (interactive)
  (let ((inhibit-read-only t)	
	(contact-buffer (get-buffer-create cliaspora-contact-buffer-name))
	)
    (with-current-buffer contact-buffer
      (cliaspora-mode)
      (cliaspora-stream-mode)
      (delete-region (point-min) (point-max))      
      )
    (cliaspora-contacts-get-contacts contact-buffer)
    (switch-to-buffer contact-buffer)
    )
  )


(defun cliaspora-contacts-show (json-parsed-contact)
  "Print a contact in the current buffer according to the JSON parsed element.

JSON-PARSED-CONTACT is a parsed part of the JSON readed by `json-read' that corresponds to the contact. Usually is a list of const."
  (let ((url (cliaspora-url (cdr (assoc 'url json-parsed-contact))))
	(handle (cdr (assoc 'handle json-parsed-contact))) ;; Usually is the cliaspora address (name@joincliaspora.com for example)
	(avatar (cdr (assoc 'avatar json-parsed-contact)))
	(name (cdr (assoc 'name json-parsed-contact)))
	(guid (cdr (assoc 'guid json-parsed-contact)))
	(id (cdr (assoc 'id json-parsed-contact)))
	)
    (insert (concat
	     (propertize 
	      "          ====================          \n"
	      'cliaspora-message-separator t)))
    (insert (format "![%s](%s)" name avatar) "\n")
    (insert (propertize
	     (format "%s (%s):" name handle) 
	     'cliaspora-is-user-name t)
	    "\n")
    (insert (format "[%s](%s)" url url) "\n")
    (insert (format "GUID: %s" guid) "\n")
    (insert (format "ID: %s" id) "\n")    
    (insert (cliaspora-add-link-to-userstream "See his/her stream" (cliaspora-get-username handle))
	    "\n"
	    )
    )
  )

(defvar cliaspora-show-userstream-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-contacts-show-userstream-key)
    (define-key map [mouse-2] 'cliaspora-contacts-show-userstream-key)
    map)
  "Keymap used for getting a userstream."
  )

(defun cliaspora-add-link-to-userstream (text username)
  "Return a propertized text with a link to a user-stream.
USERNAME must be only the username(foo) not all the complete cliaspora-id(foo@joincliaspora.com)."
  (propertize
   text
   'mouse-face 'cliaspora-mouse-highlight-face
;   'face "link"
   'keymap cliaspora-show-userstream-map
   'cliaspora-username username
   'cliaspora-is-link-to-pub t
   'help-echo "Click here to see her/his stream.")
  )

(defun cliaspora-contacts-show-userstream-key (&rest r)
  "Find the neareset 'cliaspora-username property and get the user-stream by its username value."
  (interactive)
  (cliaspora-get-stream-by-username (cliaspora-contacts-get-username-near-point))
  )

(defun cliaspora-contacts-get-username-near-point ()
  "Get the 'cliaspora-username property's username value that are near the current-point."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'cliaspora-username))
		     'cliaspora-username)
  )
  

(defun cliaspora-contacts-insert-finale (contacts-amount)
  "Insert at the end more information regarded to the contacts list."
    (insert (concat
	     (propertize 
	      "          ====================          \n"
	      'cliaspora-message-separator t)))
    (insert (concat
	     (propertize 
	      "          ====================          \n"
	      'cliaspora-message-separator t)))
    (insert (format "Amount of Contacts Listed: %s" contacts-amount))
    )
	    
(defun cliaspora-contacts-parse-json-and-insert (buffer-from buffer-to)
  "Parse a JSON text in a buffer(BUFFER-FROM) and print the result contacts in another buffer(BUFFER-TO)

BUFFER-FROM has a JSON text as fetched from the contact's URL(without HTTP headers, there must be just the JSON text!).

BUFFER-FROM is the buffer where to take the JSON text.
BUFFER-TO is the buffer where to print the contacts."
  (with-current-buffer buffer-from
    (goto-char (point-min))
    (let* ((json-elts (json-read))
	   (le (length json-elts))
	   )
      (with-current-buffer buffer-to
	(dotimes (i le)	  	  
	  (cliaspora-contacts-show (aref json-elts i))
	  )
	(cliaspora-contacts-insert-finale le)
	)
      )
    )
  )

(defun cliaspora-contacts-get-contacts (buffer-to) 
  "Retrieve contacts from D* and write it down into a buffer(BUFFER-TO) in a formated way.

Use `cliaspora-contact-url' for getting the URL where to find JSON information.

Get any information necessary as well(like username, password and authenticity token)."
  (cliaspora-ask)
  (cliaspora-get-authenticity-token-if-necessary)
  (with-current-buffer (cliaspora-get-url (cliaspora-url-json cliaspora-contact-url))
    (cliaspora-delete-http-header)
    (cliaspora-contacts-parse-json-and-insert (current-buffer) buffer-to)
    )
  )

(defvar cliaspora-contacts-all-contacts nil
  "An alist of contacts names and its usernames@pods.

Use `cliaspora-contacts-get-all-contacts' to set this variable accordingly."
  )

(defun cliaspora-contacts-get-json-info (json-parsed-contact)
  "Return the necessary info founded in a contact JSON element. 
This is usually a cons made by the name and the username@pod.

You can change this so you can have more information on each element in the `cliaspora-contacts-all-contacts' variable."
  (let ((handle (cdr (assoc 'handle json-parsed-contact))) ;; Usually is the cliaspora address (name@joincliaspora.com for example)
;;	(url (cliaspora-url (cdr (assoc 'url json-parsed-contact))))
;;	(avatar (cdr (assoc 'avatar json-parsed-contact)))
	(name (cdr (assoc 'name json-parsed-contact)))
;;	(guid (cdr (assoc 'guid json-parsed-contact)))
;;	(id (cdr (assoc 'id json-parsed-contact)))
	)
    (cons name handle)
    )
  )

(defun cliaspora-contacts-parse-json-for-contacts ()
  "Look in the JSON text for contacts and return an alist of contacts with its own complete username@pod."
  (goto-char (point-min))
  (let* ((lstout nil)
	 (json-elts (json-read))
	 (le (length json-elts))
	 )
    (dotimes (i le)
      (push (cliaspora-contacts-get-json-info (aref json-elts i)) lstout)
      )
    lstout
    )	    
  )

(defun cliaspora-contacts-get-all-contacts (&optional reload)
  "Set `cliaspora-contacts-all-contacts' if necessary looking for contacts from D*.
Return the contents of `diapsora-contacts-all-contacts'.

If RELOAD is t, then get the contacts from D* despite the variable is already setted."
  (if (or reload
	  (null cliaspora-contacts-all-contacts)
	  )
      (progn ;; Look for contacts and set the variable!
	(cliaspora-ask)
	(cliaspora-get-authenticity-token-if-necessary)
	(with-current-buffer (cliaspora-get-url (cliaspora-url-json cliaspora-contact-url))
	  (cliaspora-delete-http-header)
	  (setq cliaspora-contacts-all-contacts (cliaspora-contacts-parse-json-for-contacts))
	  )
	)
    cliaspora-contacts-all-contacts ;; the variable already has contents...
    )   
  )
  
(defun cliaspora-get-stream-by-contact (name)
  "Look for the contact stream only by its name. "
  (interactive
   (let ((string (completing-read "Contact name?" (cliaspora-contacts-get-all-contacts)))
	 )
     (list string))
   )
  
  (let ((username (cdr (assoc name cliaspora-contacts-all-contacts)))
	)
    (cliaspora-get-stream-by-username  (cliaspora-get-username username))
    )
  )

(defun cliaspora-get-stream-by-username (username)
  "Get the stream using the username. Username is the name used for login of the contact.

The `cliaspora-username-url' functions help me finding the apropiate URL."
  (interactive "sUsername?")
  
  (cliaspora-get-stream-by-name (cliaspora-username-name username))
  )

(provide 'cliaspora-contacts)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-contacts.el ends here
