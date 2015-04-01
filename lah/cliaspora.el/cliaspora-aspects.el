;;; cliaspora-aspects.el --- 
;; 
;; Filename: cliaspora-aspects.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié feb 15 12:21:12 2012 (-0300)
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
;; Aspect support for cliaspora.el
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

(require 'cliaspora-stream)

(defcustom cliaspora-aspects-stream-name
  "aspects"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the aspects stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defconst cliaspora-aspect-list-buffer-name
  "Buffer name for the list of aspects.")

(defvar cliaspora-aspect-alist nil
  "This is an alist of a pair of aspects:
 ((name of the aspect . id of the aspect) ... )

This variable will get its values using the function `cliaspora-get-aspects'.")

(defvar cliaspora-aspects-for-post nil
  "This is a list of aspects ids. This list is used for posting, and as soon as the newly posted has been sended
to the pod, the information is discarded for a new post!
This variable is intended to be as parameter for `cliaspora-post'. 
You may would like to use `cliaspora-add-aspect'.")

(defun cliaspora-look-for-aspects ()
  "Search for each aspect name an id and return an alist with all the aspects founded.  We look for the keyword \"data-aspect_id=\" and we are sure that the next line has the name with spaces."
  (let ((lista '())
        (buff (shell-command-to-string  "cliaspora list aspects")))
  
    (dolist (line-list (split-string buff "\n"))
      (let ((item-list (split-string line-list)))
       (push (cons (nth 0 item-list) (nth 1 item-list)) lista)))
    lista))


(defun cliaspora-show-all-aspects ()
  "Show all aspects in a new buffer."
  (interactive)
  (cliaspora-get-aspects)
  ;; Create (or get) and clear a new buffer
  (with-current-buffer (get-buffer-create cliaspora-aspect-list-buffer-name)
    (delete-region (point-min) (point-max))
    ;; for each element print the name...
    (dolist (elt cliaspora-aspect-alist) 
      (when  (car elt)
        (when (not (string= (car elt) "NAME"))
          (insert (car elt) " - id: " (cdr elt) "\n")))))
      
  (switch-to-buffer cliaspora-aspect-list-buffer-name))

(defun cliaspora-get-aspects (&optional reload)
  "If `cliaspora-aspect-alist hasn't been generated, get an alist of aspects as key and id as values from the Cliaspora pod and return the alist.
After generate the alist, save it in `cliaspora-aspect-alist'.
If the reload parameter is t then, no matter what `cliaspora-aspect-alist' has, reload from the `cliaspora-bookmarklet-location' URL."
  (if (or reload
          (null cliaspora-aspect-alist))
      (setq cliaspora-aspect-alist (cliaspora-look-for-aspects))))

;; We have already loaded it! return what is loaded


;; Streamming functions

(defun cliaspora-get-aspects-stream ()
  "Show the aspects stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-aspects-stream-name))
 
(defun cliaspora-get-stream-by-aspect (aspect-name)
  "Get all the message from an aspect stream called as ASPECT-NAME says."
  (interactive 
   (let ((string (completing-read "Aspect name?" (cliaspora-get-aspects)))
	 )
     (list string))
   )
  (let ((aspect-id (assoc aspect-name cliaspora-aspect-alist))
	)
    (if aspect-id
	(cliaspora-get-stream (cliaspora-url-json cliaspora-aspects-stream-name) ;;url
			     nil ;; MAX-TIME...
			     (list (cons "a_ids[]" (cdr aspect-id))) ;;GET parameters
			     )
      (message "Aspect not founded: maybe you tiped a wrong name?")
      )
    )
  )

(provide 'cliaspora-aspects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-aspects.el ends here
