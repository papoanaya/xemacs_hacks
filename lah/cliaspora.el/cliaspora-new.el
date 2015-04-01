
    ;; cliaspora-new.el
    ;; Copyright (C) 2012  Giménez, Christian N.

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;; Lunes 12 De Marzo Del 2012    

(require 'cl)

(defvar cliaspora-single-message-buffer "*cliaspora single message*"
  "Name of the buffer for a cliaspora message")

(defun cliaspora-interlace-cars (lst1 lst2)
  (if lst1
      (cons (list (car lst1) 
		   (car lst2))
	     (cliaspora-interlace-cars (cdr lst1) (cdr lst2)))
    nil))


(defun cliaspora-get-url(url)
  "Get a cliaspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "UTF-8"))))
    (url-retrieve-synchronously url)))


(defun cliaspora-json-read-url (url)
  "Returns a JSON parsed string from URL."
  (interactive)
  (let ((json-array-type 'list)
	(json-object-type 'alist)
	(http-buffer (cliaspora-get-url url)))
    (with-current-buffer http-buffer
      (cliaspora-delete-http-header)
      (let ((stream-parsed (json-read)))
	 stream-parsed))))

(defun cliaspora-parse-json-read (key &optional url)
"Get from URL the JSON part of KEY."
;  (window-configuration-to-register cliaspora-stream-register)
  (let ((lst-stream (cliaspora-extract-json key
					   (cliaspora-json-read-url  
					    (if url
						url
					      cliaspora-entry-stream-url))))
	(stream-buffer (get-buffer-create cliaspora-stream-buffer)))
    (switch-to-buffer stream-buffer)
    (delete-region (point-min) (point-max))
    (cliaspora-header-stream)
    (save-excursion
    ; Instead of (mapcar 'cliaspora-show-json-parsed-message lst-stream)
    ; which is must faster!
      (dolist (stream-posts lst-stream)
	(cliaspora-show-json-parsed-message stream-posts)))))


(defun cliaspora-show-json-parsed-message (parsed-message)
  "This is highly dependend on cliaspora* API" 
    (with-current-buffer (current-buffer)
      (let* ((message-id (cliaspora-extract-json 'id parsed-message))
	     (message-guid (cliaspora-extract-json 
			    'guid parsed-message))
	     (user-id (cliaspora-extract-json-list 
		       '(author id) parsed-message))
	     (user-guid (cliaspora-extract-json-list 
			 '(author guid) parsed-message))
	     (name (cliaspora-extract-json-list 
		    '(author name) parsed-message))
	     (cliaspora_id (cliaspora-extract-json-list 
			   '(author cliaspora_id) parsed-message))
	     (text (cliaspora-extract-json-list 
		    '(text) parsed-message))
	     (date  (cliaspora-extract-json-list 
		     '(created_at) parsed-message))
	     (provider_display_name  (cliaspora-extract-json-list 
		     '(provider_display_name) parsed-message))
	     (avatar (cliaspora-extract-json-list
		      '(author avatar small) parsed-message))
	     (photos (cliaspora-extract-json-list '(sizes large) 
						 (car (cliaspora-extract-json
						       'photos parsed-message))))
	     (amount-comments (cliaspora-extract-json-list
			       '(comments_count) parsed-message))
	     (amount-likes (cliaspora-extract-json-list
			    '(likes_count) parsed-message)))
	
	(insert  "\n---\n"
		 "![" name "](" avatar ")\n"
		 (format "%s (%s):\n %s\n" name cliaspora_id date)
		 (format "user guid: %s\n" user-guid)
		 (format "user id: %s\n" user-id)
		 (format "message id: %s\n" message-id)
		 (format "message guid: %s\n" message-guid)
		 (format "from: %s\n" provider_display_name)
;		 (format "%s\n" date)
		 (format "Has %s comments  and %s likes.\n" amount-comments amount-likes)
		 (format "%s\n\n" text))
	(if photos
	    (insert (format "![photo](%s)" photos))
	  nil))))


;(defun cliaspora-show-json-parsed-single-message ()
;  (window-configuration-to-register cliaspora-single-message-register)
;  )

;(defun cliaspora-show-json-parsed-single-message-comments ()
;  )

(defun cliaspora-stream ()
  (interactive)
  (cliaspora-ask)
  (cliaspora-authenticity-token cliaspora-sign-in-url) 
  (cliaspora-parse-json-read 'posts)
  (cliaspora-mode))


(defun cliaspora-see-regexp-markdow ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
					; tag stream
    (let ((markdown-points (cliaspora-get-all-regexp-markdown-points  cliaspora-regexp-tag 0)))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadr mpoint) (cddr mpoint)
			     (list 'mouse-face 'cliaspora-mouse-highlight-face
;				   'face "link"
				   'keymap cliaspora-show-tag-map
				   'cliaspora-tag (car mpoint)
				   'help-echo "Click here to see the tag stream in new buffer.")))))
  
  (save-excursion
    (goto-char (point-min))
					; single message
    (let ((markdown-points (cliaspora-interlace-cars
			    (cliaspora-get-all-regexp-markdown-points  "\\(message guid: \\)\\(.*\\)" 0)
			    (cliaspora-get-all-regexp-markdown-points  "\\(message guid: \\)\\(.*\\)" 1))))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadar mpoint) (cddar mpoint)
			     (list 'mouse-face 'cliaspora-mouse-highlight-face
;				   'face "link"
				   'keymap cliaspora-show-message-map
				   'cliaspora-message-id (caadr mpoint)
				   'help-echo "Click here to see the this entry with comments.")))))
  (save-excursion
    (goto-char (point-min))
					; user stream
    (let ((markdown-points (cliaspora-interlace-cars 
 			    (cliaspora-get-all-regexp-markdown-points  cliaspora-regexp-user-entry 0)
			    (cliaspora-get-all-regexp-markdown-points  cliaspora-regexp-user-id 1))))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadar mpoint) (cddar mpoint)
			     (list 'mouse-face 'cliaspora-mouse-highlight-face
;;				   'face "link"
				   'keymap cliaspora-show-guid-user-map
				   'cliaspora-user-guid (caadr mpoint)
				   'help-echo "Click here to see this user stream in new buffer."))))))
  
(defvar cliaspora-show-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-show-single-message)
    (define-key map [mouse-2] 'cliaspora-show-single-message)
    map)
  "")





(defun cliaspora-show-single-message ()
  (let ((message-id
	 (get-text-property (+ 1 (previous-single-property-change (point) 'cliaspora-message-id))
			    'cliaspora-message-id)))
    (cliaspora-get-single-message message-id)))

;(cliaspora-get-single-message "1263970")


(defun  cliaspora-get-guid-user-stream (guid)
  ""
  (interactive)  
  (cliaspora-ask)
  (cliaspora-authenticity-token cliaspora-sign-in-url)
  (save-excursion
    (cliaspora-parse-json-read 'posts  (concat "https://" cliaspora-pod "/people/" guid ".json"))
    (cliaspora-mode)))



(defun cliaspora-get-entry-stream-tag (tag)
  ""
  (interactive "s")
    (cliaspora-ask)
    (cliaspora-authenticity-token cliaspora-sign-in-url)
    (save-excursion
      (cliaspora-parse-json-read 'posts  (concat "https://" cliaspora-pod "/tags/" tag ".json"))
      (cliaspora-mode)))

(defun cliaspora-get-single-message (message-id &optional commentp)
  "This does not work..."
  (interactive)
  (window-configuration-to-register cliaspora-single-message-register)
  (cliaspora-ask)
  (cliaspora-authenticity-token cliaspora-sign-in-url)
  (save-excursion
    (let ((buffer-message (get-buffer-create cliaspora-single-message-buffer)))
      (switch-to-buffer buffer-message)
      (with-current-buffer buffer-message
	(cliaspora-show-json-parsed-message 
	 (car (cliaspora-json-read-url
	       (format "%s/%s.json" cliaspora-single-message-url message-id))))
	(switch-to-buffer-other-window buffer-message)
;    (concat "https://" cliaspora-pod "/" message-id ".json"))))
	(cliaspora-mode)))))
      
(defvar cliaspora-regexp-user-guid "^user guid: \\(.*\\)"
  )

(defvar cliaspora-regexp-user-id "^user id: \\(.*\\)"
  )

(defvar cliaspora-regexp-message-guid "^message guid: \\(.*\\)"
  )
(defvar cliaspora-regexp-message-id "^message id: \\(.*\\)"
  )

(defun cliaspora-shortenurl-replace-at-point ()
  (interactive)
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (short-url (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))

(defun cliaspora-post-comment (post id)
  "Does not work!!!"
  "Post POST to cliaspora."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" cliaspora-username)
			  (cons "user[password]" cliaspora-password)
			  (cons "authenticity_token" cliaspora-auth-token)
			  (cons "text" post)
			  (cons (concat "new_comment_on_" id) id)
			  (cons (concat "comment_text_on_" id) id))
		    "&")))
    (url-retrieve (concat "https://joincliaspora.com/posts/" id)
		  (lambda (arg) 
		    (switch-to-buffer (current-buffer))))))

;action="/posts/{{id}}/comments" class="new_comment" id="new_comment_on_{{id}}" method="post">
;      {{#with current_user}}

;(cliaspora-authenticity-token "https://joincliaspora.com/posts/1257932")
;(cliaspora-post-comment "no-comments" "1257932")

;(concat "https://joincliaspora.com/posts/" "1257932")
;"https://joincliaspora.com/posts/1257932"

				 


(defvar cliaspora-show-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-show-tag-stream)
    (define-key map [mouse-2] 'cliaspora-show-tag-stream)
    map)
  "")


(defun cliaspora-show-tag-stream (&rest r)
  (interactive)
  (let ((tag
	 (get-text-property (+ 1 (previous-single-property-change (point) 'cliaspora-tag))
			    'cliaspora-tag)))
    (cliaspora-get-entry-stream-tag  (cliaspora-markdown-tag-strip tag))))


(defvar cliaspora-show-guid-user-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-show-user-stream)
    (define-key map [mouse-2] 'cliaspora-show-user-stream)
    map)
  "")

(defvar cliaspora-show-video-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'goto-address-at-point)
    (define-key map [mouse-2] 'goto-address-at-point)
    map)
  "")


(defun cliaspora-header-stream ()
  (insert (propertize
  	   (format "%s" "Extrapolation")
  	   'mouse-face 'cliaspora-mouse-highlight-face
  	   'face cliaspora-header-face-1
  	   'keymap cliaspora-stream-header-map
  	   'help-echo "Click here to see this cliaspora stream.")
  	  " ")
  (insert (propertize
  	   (format "%s" "Pinboard")
  	   'mouse-face 'cliaspora-mouse-highlight-face
  	   'face cliaspora-header-face-1
  	   'keymap cliaspora-likes-header-map
  	   'help-echo "Click here to see this cliaspora stream.")
  	  " ")
  (insert
	  (propertize
	   (format "%s" "search tag")
	   'mouse-face 'cliaspora-mouse-highlight-face
	   'face cliaspora-header-face-1
	   'keymap cliaspora-stream-header-tag-map
	   'help-echo "Click here to see this cliaspora stream tag (tag will be prompted).")))


(defvar cliaspora-stream-header-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-get-entry-stream-tag)
    map)
  "")


(defvar cliaspora-likes-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-stream-likes)
    map)
  "")


(defvar cliaspora-stream-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-stream)
    map)
  "")


(defun cliaspora-show-user-stream (&rest r)
  (interactive)
  (let ((guid-user
	 (get-text-property (+ 1 (previous-single-property-change (point) 'cliaspora-user-guid))
			    'cliaspora-user-guid)))
    (cliaspora-get-guid-user-stream  guid-user)))

(defun cliaspora-show-videos ()
  (interactive)
  (let ((markdown-points (cliaspora-get-all-regexp-markdown-points cliaspora-regexp-youtube-link)))
    (dolist (mpoint markdown-points)
      (add-text-properties (cadr mpoint) (cddr mpoint)
			   (list 'mouse-face 'cliaspora-mouse-highlight-face
;;				 'face "link"
				 'keymap cliaspora-show-video-map
				 'help-echo "Click here to see the this video on a external browser.")))))




(provide 'cliaspora-new)


(defun cliaspora-send-comment-post (comment post-id)
  "Send a comment for the post given by the post-id.
Comment should be a String and post-id the id number of the post."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" cliaspora-username)
			  (cons "user[password]" cliaspora-password)
			  (cons "text" comment)
			  (cons "user[remember_me]" "1")
			  (cons "authenticity_token" cliaspora-auth-token)
			  (cons "commit" "Sign in"))
		    "&")))
    (url-retrieve-synchronously (concat (cliaspora-post-comment-url post-id)))))

(defun cliaspora-post-comment-url (post-id)
  "Return the URL for posting a comment for the post with id post-id"
  (cliaspora-url
   (format "%s/%s/%s"
	   cliaspora-single-message-url
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   cliaspora-comment-name)))

(defconst cliaspora-comment-buffer-name "*cliaspora comment*"
  "This is the name of the comment buffer.")

(defvar cliaspora-comment-buffer nil
  "This is the buffer (supposed to be only one or unique) for write a comment.")

(defun cliaspora-new-comment-buffer (post-id)
  "Create a new buffer for write a comment for the post with id given by post-id."
  (interactive)
  (setq cliaspora-next-comment-to-post post-id)
  ;; create buffer
  (setq cliaspora-comment-buffer (get-buffer-create cliaspora-comment-buffer-name))
  (switch-to-buffer-other-window cliaspora-comment-buffer)
  ;; insert header and footer, modes... etc.
  (cliaspora-date)
  (insert cliaspora-footer-post)
  (goto-char (point-min))
  (insert cliaspora-header-post)
  (cliaspora-mode)
  (message "Use C-c C-c to comment to cliaspora or use cliaspora-send-comment-this-buffer."))

(defvar cliaspora-next-comment-to-post
  nil
  "This is the post id where to send the comment in the next `cliaspora-send-comment-this-buffer' function call.")

(defun cliaspora-url (location)
  "Make the URL according to the `cliaspora-pod'(pod selected)."
  (format "%s://%s/%s"
	  (if t
	      "https"
	    "http")
	  cliaspora-pod
	  location))

(defcustom cliaspora-comment-name
  "comments"
  "This is the name of the comments for posting."
  :type 'string
  :group 'cliaspora-streams)

(defun cliaspora-send-comment-this-buffer ()
  "Send this buffer as a comment to the post determined by the id `cliaspora-next-comment-to-post'."
  (interactive)
  (cliaspora-ask)
  (when (null cliaspora-auth-token)
    (message (concat "Getting authenticity token..."))
    (cliaspora-authenticity-token (cliaspora-url cliaspora-sign-in-url))
    (message (concat "done: " cliaspora-auth-token)))
  (cliaspora-send-comment-post (buffer-string) cliaspora-next-comment-to-post))