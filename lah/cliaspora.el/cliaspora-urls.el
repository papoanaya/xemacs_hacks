;;; diaspora-urls.el --- 
;; 
;; Filename: diaspora-urls.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr  4 11:52:27 2012 (-0300)
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
;; This library provides functions to create URLs related to Cliaspora.
;;
;; Please, don't create URLs using `concat' or other function! Create your function here if necessary and call it from here.
;; This is necessary because URLs must have the `cliaspora-pod' and other suitable elements.
;;
;; Make sure that always calls this functions for an URL and don't create any URL in others libraries... 
;; it leads to inconcistences when creating URLs.
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

;; It is a user facility that `cliaspora-secure-pod' and `cliaspora-pod' is added in the customization group "cliaspora".

(defcustom cliaspora-secure-pod
  t
  "If your cliaspora pod use https, set this to true.
If only use http, use false."
  :type 'boolean
  :group 'cliaspora)

(defcustom cliaspora-pod 
  "joindiaspora.com"
  "Your cliaspora* pod."
  :type 'string
  :group 'cliaspora)

(defgroup cliaspora-urls nil
  "Cliaspora-urls URL Generator for Cliaspora."
  :group 'cliaspora
  :tag "Cliaspora's URLs Generation."
  :version "23.0")

(defcustom cliaspora-sign-in-url   
  "/users/sign_in"
  "URL used to signing in."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-bookmarklet-location
  "/bookmarklet"
  "Location of the bookmarklet. This is used in `cliaspora-get-aspects' for searching for the aspects.
A bit complicated but the only way known to get a list of aspects."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-status-messages-url 
  "/status_messages"
  "URL used to update cliaspora status messages."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-single-message-url
  "/posts"
  "URL used to get a single message."
  :type 'string
  :group 'cliaspora-streams)


(defcustom cliaspora-likes-name
  "likes"
  "Name for sending likes POSTs."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-userstream-url
  "/u"
  "This is the rest of the URL for getting a user stream. The first part is usually taken from `cliaspora-pod' variable.

See `cliaspora-url' and `cliaspora-url-json'."
  :type 'string
  :group 'cliaspora-url
  )

(defcustom cliaspora-messages-url "/conversations"
  "This is the URL part that corresponds to the conversation stream."
  :group 'cliaspora-url
  :type 'string
  )

(defcustom cliaspora-notifi-url "notifications"
  "This is the URL part that corresponds to the notifications."
  :group 'cliaspora-url
  :type 'string
  )

(defun cliaspora-url (location)
  "Make the URL according to the `cliaspora-pod'(pod selected)."
  (format "%s://%s/%s" 
	  (if cliaspora-secure-pod
	      "https"
	    "http")
	  cliaspora-pod 
	  location))


(defun cliaspora-url-json (location)
  "Make the URL as in `cliaspora-url' but for retrieving JSON formats pages, according to the `cliaspora-pod' (pod selected)."
  (cliaspora-url
   (format "%s.json" location)))


(defun cliaspora-username-name (username)
  "Return the last part of the URL necessary to complete the URL with `cliaspora-url' or `cliaspora-url-json'."
  (format "%s/%s" cliaspora-userstream-url username)
  )


(defun cliaspora-post-comment-url (post-id)
  "Return the URL for posting a comment for the post with id post-id"
  (cliaspora-url 
   (format "%s/%s/%s"
	   cliaspora-single-message-url
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   cliaspora-comment-name)))

(defun cliaspora-get-comment-url (post-id)
  (cliaspora-url
   (format "%s/%s/%s.json" cliaspora-single-message-url 
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   cliaspora-comment-name)))

(defun cliaspora-image-path (image-name)
  "Return the temporal image path."
  (concat cliaspora-image-directory 
	  (cliaspora-image-filter-out-bad-chars image-name))
  )

(defun cliaspora-image-filter-out-bad-chars (image-name)
  (let ((name image-name))	
    (while (string-match "[&\\?><|%]" name)
      (setq name (replace-match "" nil t name))
      )
    name
    )
  )
  

(defun cliaspora-image-path-from-url (image-url &optional user-id)
  "Return the temporal image path from the url where it has been dowloaded."
  (concat cliaspora-image-directory
	  (if user-id 
	      (concat user-id "-"))
	  (cliaspora-image-filter-out-bad-chars (file-name-nondirectory image-url))
	  )
  )


(defun cliaspora-likes-url (post-id)
  (cliaspora-url
   (format "%s/%s/%s"
	   cliaspora-single-message-url
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   cliaspora-likes-name)
   )
  )

(defun cliaspora-messages-url (message-id)
  (cliaspora-url
   (format "%s/%s"
	   cliaspora-messages-url
	   (if (numberp message-id)
	       (number-to-string message-id)
	     message-id)
	   )
   )
  )


(defun cliaspora-notif-url (notification-id)
  (cliaspora-url
   (format "%s/%s"
	   cliaspora-notifi-url
	   (if (numberp notification-id)
	       (number-to-string notification-id)
	     notification-id)
	   )
   )
  )

(defun cliaspora-image-url (pending aspect-ids-list image-name)
  (cliaspora-url
   (concat
    "/photos?"
    (mapconcat (lambda (arg)
		 (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
	       (append
		(list (cons "photo[pending]" (if pending
						 "true"
					       "false"))
		      (cons "set_profile_image" nil)
		      (cons "qqfile" image-name))
		(cliaspora-image-aspect-list aspect-ids-list))
	       "&")
    )    
   )
  )
  

(defun cliaspora-image-aspect-list (aspect-ids-list)
  (let ((aspect 0)
	(outlst nil)
	)
    (dolist (e aspect-ids-list)
      (push (cons (format "photo[aspect_ids][%s]"  aspect) e) 
	    outlst)		    
      (setq aspect (+ 1 aspect))
      )
    outlst
    )
  )

(provide 'cliaspora-urls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-urls.el ends here
