;;; cliaspora.el --- Simple Emacs-based client for cliaspora*

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

;; Streaming 

(require 'cl)
(require 'cliaspora-comments)

                                        ; ********************
                                        ; Customization

(defgroup cliaspora-streams nil
  "URL and names for the Streams used in cliaspora.el."
  :group 'cliaspora-urls
  :version "23.0"
  :tag "cliaspora streams urls")

(defcustom cliaspora-timezone -3
  "Amount of hours as a timezone. If your timezone is -3UTC then use -3(three hours less to reach the UTC!)"
  :group 'cliaspora
  :type 'integer)

(defcustom cliaspora-get-always-authenticity-token t
  "Always get the authenticity token when connecting to Diáspora. 
You may would like to get only one authenticity token, but sometimes posting or getting info may fail.

If you set this to true, then cliaspora.el will always get the authenticity token, making it more slow but robust.

Note: If you have a slow Internet, you may would like to set this into false(or nil)."
  :group 'cliaspora-streams
  :type 'boolean)

(defcustom cliaspora-participate-stream-name
  "participate"
  "Name of the \"Participate\" stream. 
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-explore-stream-name
  "explore"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the entry stream or explore stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-entry-stream-name 
  "stream"
  "JSON version of the entry stream(the main stream)."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-public-stream-name
  "public"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the public stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-followed-tags-stream-name
  "followed_tags"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the followed tags stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-mentions-stream-name
  "mentions"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the mentions stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-liked-stream-name
  "liked"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the liked stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-commented-stream-name
  "commented"
  "This is the name (as appear in cliaspora/config/routes.rb in the cliaspora project) of the commented stream.
This is the name of the page, for example:
If `cliaspora-pod' has the value \"joincliaspora.com\", then,
the JSON page is at the URL:
  https://joincliaspora.com/participate.json

And the `cliaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'cliaspora-streams)

(defcustom cliaspora-image-external-program "eog"
  "This is the program path and name. If you want to see an image in an external program this must be
setted correctly."
  :group 'cliaspora
  :type 'string)

                                        ; ********************
                                        ; Constants  

(defconst cliaspora-stream-buffer "*cliaspora stream*"
  "The name of the cliaspora stream buffer.")

                                        ; ********************
                                        ; Internal Variables

(defvar cliaspora-stream-last-post-date nil
  "A list with two (or more) elements in the format like `current-time'.

  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent."
  )

(defvar cliaspora-last-stream-visited nil
  "A list with two elements: the stream name and the max-time.

If max-time is nil, then the max-time is the current-time.

If this variable is nil then there was no last stream visited.")

                                        ; ********************
                                        ; Functions

(defun cliaspora-show-stream (status &optional new-buffer-name)
  "Show what was recieved in a new buffer.
If new-buffer-name is given then, the new buffer will have that name, 
if not, the buffer called \"Diáspora Stream\" will be re-used or created if needed."
  ;; new-buffer-name has been given? if not, use `cliaspora-stream-buffer´ as name.
  (unless new-buffer-name
    (setq new-buffer-name cliaspora-stream-buffer))
  (let ((buffer (get-buffer-create new-buffer-name))
        (text (buffer-string))
        (buf-kill (current-buffer)))    
    ;; copy text and switch
    (switch-to-buffer buffer)
    (insert text)    
    ;; kill the http buffer
    (kill-buffer buf-kill)))

(defun cliaspora-get-time-by-timezone (max-time)
  "Return the time in seconds from the epoch modified according to the timezone specified by `cliaspora-timezone' to represents the time
in the UTC standard.

This is usefull for giving this as a GET(or POST) \"max_time\" parameter for any stream."
  (+ (float-time max-time) (* cliaspora-timezone 3600));; 3600 is one hour.
  )


(defun cliaspora-get-url-entry-stream (url &optional max-time lst-get-parameters lst-post-parameters)
  "Get the Diáspora URL and leave it in a new buffer.
Returns: A new buffer where is all the information retrieved from the URL."
  (let ((url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept-Language" . "en")
           ("Accept-Charset" . "utf-8")))
        (buffer-file-coding-system 'utf-8))
    (if max-time
	
        (let ((url-request-data;; the interval of time has been setted
               (mapconcat (lambda (arg)
                            (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
                          (append (list (cons "max_time" (number-to-string (cliaspora-get-time-by-timezone max-time))))
                                  lst-get-parameters
                                  lst-post-parameters)
                          "&"))
              )
          (url-retrieve-synchronously url))

      (let ((url-request-data;; there is no interval of time
             (mapconcat (lambda (arg)
                          (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
                        (append lst-get-parameters lst-post-parameters)
                        "&"))
            )
        (url-retrieve-synchronously url)))))

(defun cliaspora-get-next-oldies ()
  "Get the next olds post of the last visited stream.

I use the `cliaspora-stream-last-post-date' variable.

This is the same as going up to the bottom of the page and let cliaspora reload the older posts."
  (interactive)
  (if cliaspora-stream-last-post-date
      (progn
        (cliaspora-visit-last-stream cliaspora-stream-last-post-date)
        )
    (message "You need to get a stream: there is no last stream visited!")
    )
  )

(defun cliaspora-visit-last-stream (&optional other-max-time)
  "Visit the last stream, maybe with max-time changed.

OTHER-MAX-TIME is a list with two elements:
  (HIGH LOW)
This is a timestamp as `current-time' returns.

Is used for getting the posts created up to that time.

I use `cliaspora-last-stream-visited' variable for getting the name of the last stream visited."
  (interactive)
  (if cliaspora-last-stream-visited     
      (cliaspora-get-stream-by-name (car cliaspora-last-stream-visited) (or other-max-time;; use other-max-time if setted
                                                                            (nth 1 cliaspora-last-stream-visited)))
    (message "There's no last stream visited."))
  )
  

(defun cliaspora-get-stream(stream-url max-time &optional lst-get-parameters lst-post-parameters)
  "Get the stream given by the url, and then, show it in the cliaspora buffer.
I expect to be logged in, but if not, I download the authenticity token.

Set MAX-TIME with a valid emacs timestamp to fetch information from and until that interval of time.

Use LST-GET-PARAMETERS to give special GET parameters to the STREAM-URL.
Same as LST-POST-PARAMETERS."
  (cliaspora-ask);; don't forget username and password!
  (cliaspora-get-authenticity-token-if-necessary)
  ;; get the in JSON format all the data
  (let (
        (stream-buff (get-buffer-create cliaspora-stream-buffer))
        (buff (cliaspora-get-url-entry-stream stream-url max-time lst-get-parameters lst-post-parameters )))
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (cliaspora-delete-http-header)
      ;; Parse JSON...
      (let ((inhibit-read-only t))
        ;; Apply cliaspora-mode
        (with-current-buffer stream-buff	  
          (cliaspora-mode)
          (cliaspora-stream-mode)
          (cliaspora-parse-json buff stream-buff)

          (if cliaspora-show-images-by-default
              (progn
                (cliaspora-get-all-images)
                (cliaspora-show-images)))

          (cliaspora-remove-bad-chars)
          (cliaspora-replace-bad-links)
          (cliaspora-hide-markdown)

          (switch-to-buffer stream-buff)
          (goto-char (point-min))
          )
        )
      )
    ;; Delete HTTP Buffer
    ;;(kill-buffer buff)   
    )
  )

(defun cliaspora-read-date ()
  "Read a date from the minibuffer and return in the format as `current-time' or `encode-time' does."
  (let ((day 0)
        (month 0)
        (year 0)
        (hour 0)
        (min -1)
        (sec 0)
        (max-year (nth 5 (decode-time)))
        (mess "")
        )
    ;; DAY
    (setq mess "Day:")
    (while (or (< day 1)
               (> day 31))
      (setq day (read-number mess))
      (when (or (< day 1) 
                (> day 31));; Incorrect day!
        (setq mess "Please write a number between 1 up to 31. Day:")
        )
      )
    ;; MONTH
    (setq mess "Month:")
    (while (or (< month 1)
               (> month 12))      
      (setq month (read-number mess))
      (when (or (< month 1)
                (> month 12));; Incorrect month!
        (setq mess "Please write a number between 1 up to 12. Month:")
        )
      )
    ;; YEAR
    (setq mess "Year:")
    (while (or (< year 2000)
               (> year max-year))
      (setq year (read-number mess))
      (when (or (< year 2000)
                (> year max-year))
        (setq mess (concat "Please write a number between 2000 and "
                           (number-to-string max-year)
                           ". Year:"))
        )
      )
    
    ;; HOUR
    (setq mess "Hour:")
    (while (or (< hour 1)
               (> hour 24))
      (setq hour (read-number mess))
      (when (or (< hour 1)
                (> hour 24))
        (setq mess (concat "Please write a number between 1 and 24. Hours:"))
        )
      )

    ;; MIN
    (setq mess "MIN:")
    (while (or (< min 0)
               (> min 59))
      (setq min (read-number mess))
      (when (or (< min 0)
                (> min 59))
        (setq mess (concat "Please write a number between 0 and 59. Minutes:"))
        )

      )
    
    (encode-time sec min hour day month year)
    )
  )

                                        ; Streams!

(defun cliaspora-get-participate-stream ()
  "Show the participate stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-participate-stream-name))  

(defun cliaspora-get-stream (stream-name &optional max-date)
  "Show the entry stream."
  (let ((stream-buff (get-buffer-create cliaspora-stream-buffer))
        (buff (shell-command-to-string (concat   "cliaspora show " stream-name ))))
    (let ((inhibit-read-only t))
      ;; Apply cliaspora-mode
      (with-current-buffer stream-buff
       (kill-region (point-min) (point-max))
        (goto-char (point-min))
        (insert buff)
        (goto-char (point-min))
        (cliaspora-mode)
        (cliaspora-stream-mode)
        (cliaspora-remove-bad-chars)
        (cliaspora-replace-bad-links)
        (cliaspora-hide-markdown)
        (switch-to-buffer stream-buff)))))

(defun cliaspora-get-entry-stream ()
  "Show the mentions stream."
  (interactive)
  (cliaspora-get-stream "stream"))

(defun cliaspora-get-activity-stream ()
  "Show the mentions stream."
  (interactive)
  (cliaspora-get-stream "activity"))

(defun cliaspora-get-my-stream ()
  "Show the mentions stream."
  (interactive)
  (cliaspora-get-stream "mystream"))


(defun cliaspora-get-entry-stream-up-to-date ()
  "Read the max date from the user and show the stream.

In other words, look for posts up to that date."
  (interactive)
  (let ((max-date (cliaspora-read-date)))
    (cliaspora-get-entry-stream max-date)
    )
  )

(defun cliaspora-one-day-more (from-date)
  "Adds one day more at the FROM-DATE. 
FROM-DATE must be in the format like `current-date' returns:
  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent.

The return value is in the same format as the FROM-DATE parameter."
  (seconds-to-time
   (+ 86400 (float-time from-date)));; one day in seconds is 86400: add one day in seconds.   
  )

(defun cliaspora-get-entry-stream-next-oldies ()
  "Get the next olds post of the entry stream.

I use the `cliaspora-stream-last-post-date' variable.

This is the same as going up to the bottom of the page and let cliaspora reload the older posts."
  (interactive)
  (if cliaspora-stream-last-post-date
      (progn
        (cliaspora-get-entry-stream cliaspora-stream-last-post-date)
        )
    (message "You need to get the a stream: there is no last post!")
    )
  )

(defun cliaspora-get-public-stream ()
  "Show the public stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-public-stream-name))

(defun cliaspora-get-followed-tags-stream ()
  "Show the followed tags stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-followed-tags-stream-name))

(defun cliaspora-get-mentions-stream ()
  "Show the mentions stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-mentions-stream-name))

(defun cliaspora-get-liked-stream ()
  "Show the liked stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-liked-stream-name))

(defun cliaspora-get-commented-stream ()
  "Show the commented stream."
  (interactive)
  (cliaspora-get-stream-by-name cliaspora-commented-stream-name)
  )

(defun cliaspora-get-temp-path (filename)
  "Return the path of temporal files. 
Check if the temporal directory exists, if not create it."
  (unless (file-exists-p cliaspora-temp-directory)    
    (make-directory cliaspora-temp-directory))
  (format "%s/%s" cliaspora-temp-directory filename))

(defun cliaspora-change-to-html ()
  "Change current buffer from markdown into html and htmlize"
  (write-file (cliaspora-get-temp-path "entry-stream.markdown"))
  (markdown-preview))

(defvar cliaspora-show-message-map-stream
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'cliaspora-comment-message)
    (define-key map [return] 'cliaspora-show-message-new-buffer)
    (define-key map [mouse-2] 'cliaspora-show-message-new-buffer)
    map)
  "Keymap used when the user clics on a name link.")

(defvar cliaspora-like-message-map-stream
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'cliaspora-comment-message)
    (define-key map [return] 'cliaspora-like-message)
    (define-key map [mouse-2] 'cliaspora-like-message)
    map)
  "Keymap used when the user clics on a name link.")

(defvar cliaspora-stream-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-q" 'cliaspora-single-message-destroy)
    map)
  "Keymap used in the stream and messages buffers.")

(defvar cliaspora-comment-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-comment-message)
    (define-key map [mouse-2] 'cliaspora-comment-message)
    map)
  "Keymap used in the stream and messages buffers for commenting a message.")


;; A few notes about the next functiom `cliaspora-show-message`
;; date: 20120128
;;
;; It would be much easier not to insert the text with properties as is done
;; I think it is preferable to add the properties latter on; using the same type
;; of procedures that is used to insert images. Just a thought.

(defun cliaspora-show-like (parsed-like)
  "Write only one likes.

PARSED-LIKE is a JSON part of the likes array.

Modify this function if you want to show more information or show it in other way."
  (let ((author (cdr (assoc 'name (assoc 'author parsed-like))))
        (username (cdr (assoc 'cliaspora_id (assoc 'author parsed-like))))
        )
    (insert (cliaspora-add-link-to-userstream author (cliaspora-get-username username)) "|")
    )
  )

(defun cliaspora-show-all-likes (all-parsed-likes)
  "Write in the current buffer the people who likes this post."
;; (let ((le (length all-parsed-likes))
;; 	)
;;   (dotimes (i le)
;;     (cliaspora-show-like (aref all-parsed-likes i))
;;     )
;;   )
  (dolist (elt all-parsed-likes)
    (cliaspora-show-like elt)
    )
  )

(defun cliaspora-show-message (parsed-message &optional buffer show-last-three-comments)
  "Show a parsed message in a given buffer.
If buffer is nil, then use the `current-buffer'."
;; Ensure that buffer is not nil, in case is nil, buffer will be `current-buffer'.
;; debug
;  (setq aux  parsed-message)
  (let ((buffer (if (null buffer)
                    (current-buffer)
                  buffer)))
    (with-current-buffer buffer
      (let* ((id (cdr (assoc 'id parsed-message)))
             (name (cliaspora-extract-json-list 
                    '(author name) parsed-message))
             (cliaspora_id (cliaspora-extract-json-list 
                            '(author cliaspora_id) parsed-message))
             (text (cliaspora-extract-json-list 
                    '(text) parsed-message))
             (date  (cliaspora-extract-json-list 
                     '(created_at) parsed-message))
             (avatar (cliaspora-extract-json-list
                      '(author avatar small) parsed-message))
             (photos (cdr (assoc 'photos parsed-message)))
             (amount-comments (cliaspora-extract-json-list
                               '(comments_count) parsed-message))
             (amount-likes (cliaspora-extract-json-list
                            '(likes_count) parsed-message))
             (likes (cdr (assoc 'likes parsed-message)))
             (public (cdr (assoc 'public parsed-message)))
             (provider-name (cdr (assoc 'provider_display_name parsed-message)))
             )
	
        (insert (concat
                 (propertize 
                  "          ====================          \n"
                  'cliaspora-message-separator t)))
        (insert "![" name "](" avatar ")\n")
        (insert (propertize
                 (format "%s (%s):" name cliaspora_id)
                 'cliaspora-is-user-name t)
                "\n")
        (insert (format "%s\n" date))
        (insert (propertize
                 (format "Has %s comments. %s likes." amount-comments amount-likes)
                 'cliaspora-is-amount-comments t)
                "\n")
        (when likes 
          (insert "Who likes this:\n")
          (cliaspora-show-all-likes likes)
          (insert "\n")
          )
        (insert (cliaspora-add-comment-link "Comment" id)
                " | "	 
                (cliaspora-add-link-to-publication "Read in new buffer" id)
                " | "
                (cliaspora-add-like-link "I like it!" id)
                "\n")
        (insert (format "%s\n\n" text))
        (if (equal (length photos) 0) ""
          (cliaspora-insert-photos-markdown photos))	
        (if (equal public t)
            (insert "Public")
          (insert "Limited")
          )

        (if provider-name
            (insert (format " - Published using *%s*\n" provider-name))
          (insert " - Published using web(or there's no provider name!)\n")
          )
	  
        (when show-last-three-comments
          (insert  "\n"
                   (propertize 
                    "Comments:"
                    'cliaspora-comments-start t)
                   "\n")
          (cliaspora-comments-show-last-three parsed-message)
          (insert "\n")
          )
        )      
      )    
    )  
  )

(defun cliaspora-insert-photos-markdown (photos &optional buffer)
  "Insert photos in markdown format.

PHOTOS may be an array or just an element of a JSON parsed message.

For some reason Cliaspora return two tipes of photos fields in the JSON message:
 * One `json-read' returns it as an array.
 * The other `json-read' returns it as a list.
This parses the two options!"
  (cond 
   ((arrayp photos);; is a stream message JSON photo field!
    (let ((le (length photos));; is an array... is a different entry!
          (i 0))
      (dotimes (i le)	  
        (insert "![photo](" 
                (cdr (assoc 'large (assoc 'sizes (aref photos i))))
                ")\n")))
    )
   ((listp photos);; Is a single message JSON photo field!
    (dolist (photo photos)
      (insert "![photo](" 
              (or (cdr (assoc 'large (assoc 'sizes (car photo))))
                  (cdr (assoc 'large (assoc 'sizes photo))))
              ")\n"))
    )
   )
  )

(defun cliaspora-add-comment-link (text id-message)
  "Return a propertized text with a link to publication. Ready to use with a map like `cliaspora-show-message-map'
or a function like `cliaspora-show-message-new-buffer'."
  (propertize
   text
   'mouse-face 'cliaspora-mouse-highlight-face
;     'face "link"
   'keymap cliaspora-comment-message-map
   'cliaspora-is-link-to-pub t
   'cliaspora-id-message id-message
   'help-echo "Click here to comment this message in new buffer.")
  )

(defun cliaspora-add-link-to-publication (text id-message)
  "Return a propertized text with a link to publication. Ready to use with a map like `cliaspora-show-message-map'
or a function like `cliaspora-show-message-new-buffer'."
  (propertize
   text
   'mouse-face 'cliaspora-mouse-highlight-face
;   'face "link"
   'keymap cliaspora-show-message-map-stream
   'cliaspora-id-message id-message
   'cliaspora-is-link-to-pub t
   'help-echo "Click here to see this message in new buffer.")
  )
(defun cliaspora-add-like-link (text id-message)
  "Return a propertized text with a link for sending a \"like\". Ready to use with a map like `cliaspora-like-message-map-stream'."
  (propertize
   text
   'mouse-face 'cliaspora-mouse-highlight-face
;   'face "link"
   'keymap cliaspora-like-message-map-stream
   'cliaspora-id-message id-message
   'cliaspora-is-like-link t
   'help-echo "Click here to declare that I like this post!")  
  )

(defun cliaspora-get-id-message-near-point ()
  "Get the cliaspora-id-message property value searching from point.
Use it for getting the nearest id post number when selecting a message."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'cliaspora-id-message))
                     'cliaspora-id-message))


(defun cliaspora-show-message-new-buffer (&rest r)
  "Show this message in new buffer. Load the message, and all its comments, and show it!."
  (interactive)
  (cliaspora-get-single-message (cliaspora-get-id-message-near-point)))

(defun cliaspora-comment-message-new-buffer (&rest r)
  "Create a new buffer for commenting the current message."
  (interactive)
  (cliaspora-new-comment-buffer (cliaspora-get-id-message-near-point)))

(defun cliaspora-single-message-destroy ()
  "Destroy the current cliaspora single message buffer."
  (interactive)
  (when (equal cliaspora-single-message-buffer (buffer-name))
    (kill-buffer (current-buffer))
    (jump-to-register cliaspora-single-message-register)))


(defun cliaspora-get-single-message (id-message)
  "Get from the `cliaspora-single-message-url' URL the given message by id."
  (window-configuration-to-register cliaspora-single-message-register)
  (let ((buff (get-buffer-create cliaspora-single-message-buffer))
        (buff-http (cliaspora-get-url-entry-stream
                    (format "%s/%s.json" (cliaspora-url cliaspora-single-message-url) id-message))))
    (with-current-buffer buff-http
      ;; Delete HTTP header!
      (cliaspora-delete-http-header))
    (let ((inhibit-read-only t))
      (cliaspora-parse-single-message-json buff-http buff nil)
      (cliaspora-insert-comments-for-message id-message buff)
      )
    (switch-to-buffer-other-window buff)
;    (switch-to-buffer buff)
    (with-current-buffer buff
      (cliaspora-mode)
      (cliaspora-stream-mode)
      )
    )
  )


;; images: needs working

(defun cliaspora-get-user-avatar (url &optional user-id)
  (let ((url-request-method "GET")
        (url-show-status nil))
    (url-retrieve url 'cliaspora-write-image
                  (list url user-id))))
				 
(defun cliaspora-get-image (url)
  (let ((url-request-method "GET")
        (url-show-status nil))
    (url-retrieve url 'cliaspora-write-image
                  (list url))))

(defun cliaspora-get-image-sync (url)
  "Same as `cliaspora-get-image' but synchronously."
  (let ((url-request-method "GET")
        (url-show-status nil))
    (with-current-buffer (url-retrieve-synchronously url)
      (cliaspora-write-image nil url))))


(defun cliaspora-write-image (status url &optional user-id)
  (let ((image-file-name
         (cliaspora-image-path-from-url url user-id))
        (end-image (search-forward "\C-j\C-j" nil t))
        )
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (when end-image
      (delete-region (point-min) (search-forward "\C-j\C-j" nil t))
      )
    (save-buffer 0)
    (kill-buffer (current-buffer)))
  )

(defun cliaspora-get-all-images ()
  (interactive)
  (mapcar 'cliaspora-get-image-sync (cliaspora-get-all-image-links)))

(defun cliaspora-show-images ()
  "Shows images in buffer."
  (interactive)
  (save-excursion
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          (modified-p (buffer-modified-p))
          deactivate-mark
          (images-points (cliaspora-get-all-regexp-markdown-points cliaspora-regexp-image)))
      (dolist (ipoint images-points)
        (cliaspora-insert-image (cadr ipoint) (cddr ipoint)))
      (goto-char (point-min)))))

(defun cliaspora-insert-image (beg end)
  "Create an image  and insert it place of an `cliaspora-regexp-image' defined by BEG and END."
  (condition-case nil
      (add-text-properties (cadr ipoint) (cddr ipoint)
                           (list 'display (create-image (cliaspora-image-path-from-url (car ipoint))))
                           )
    (error nil));; Don't throw errors... creating an image that emacs doesn't understand creates an error.
  )

(defun cliaspora-get-and-show-images (&rest r)
  "Look for all images URL and download all of them into the temporary directory, then insert them in the current buffer where those URLs are."
  (interactive)
  (let ((inhibit-read-only t))    
    (cliaspora-get-all-images)
    (cliaspora-show-images)
    )
  )
  

(defun cliaspora-unshow-images ()
  "Un shows images in buffer."
  (interactive)
  (save-excursion
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          (modified-p (buffer-modified-p))
          deactivate-mark)
      (unwind-protect
          (remove-text-properties (point-min) (point-max)
                                  '(display)))
      (set-buffer-modified-p modified-p))))


(defun cliaspora-get-all-regexp-markdown-points (regexp &optional opt)
  (save-excursion
    (cond ((search-forward-regexp regexp (point-max) t)
           (cons (cons (match-string-no-properties 
                        (if (not opt) 2
                          opt))
                       (cons (match-beginning 0) 
                             (match-end 0)))
                 (cliaspora-get-all-regexp-markdown-points regexp
                                                           (if (not opt) 2
                                                             opt))))
          (t nil))))

(defvar cliaspora-user-image-dir ""
  ""
  )

(defun cliaspora-show-videos (&optional opt)
  ""
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (let ((images-points (cliaspora-get-all-regexp-markdown-points cliaspora-regexp-youtube-link)))
      (dolist (ipoint images-points)
        (if (not opt)
            (add-text-properties (cadr ipoint) (cddr ipoint)
                                 (list 'display (create-image 
                                                 (concat cliaspora-user-image-dir "/" 
                                                         "video.png"))))
          (remove-text-properties (cadr ipoint) (cddr ipoint)
                                  '(display)))))))



(defun cliaspora-find-image-links ()
  "Search for all strings that matchs `cliaspora-regexp-image' from point until the end, in other words: search for all links from here."  
  (cond ((search-forward-regexp cliaspora-regexp-image (point-max) t)
         (cons (match-string-no-properties 2)
               (cliaspora-find-image-links)))
        (t nil)
        ) 
  )

(defun cliaspora-get-all-image-links ()
  "Return all image links in the current buffer.
Image links must match the regexp in `cliaspora-regexp-image'."
  (goto-char (point-min))
  (save-excursion
    (remove-duplicates (cliaspora-find-image-links) :test 'equal)
    )
  )

(defun cliaspora-get-image-link-at-point ()
  "Get the image near the point"
  (save-excursion
    (goto-char (point-at-bol))
    (when (search-forward-regexp cliaspora-regexp-image nil t)
      (match-string-no-properties 2))))  

(defun cliaspora-see-regexp-markdow ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((markdown-points (cliaspora-get-all-regexp-markdown-points  cliaspora-regexp-tag 0)))
      (dolist (mpoint markdown-points)
        (add-text-properties (cadr mpoint) (cddr mpoint)
                             (list 'mouse-face 'cliaspora-mouse-highlight-face
;				   'face "link"
                                   'keymap cliaspora-show-tag-map
                                   'cliaspora-tag (car mpoint)
                                   'help-echo "Click here to see the tag stream in new buffer."))
        ))))

;; (goto-char (point-min))
;; (let ((markdown-points (cliaspora-get-all-regexp-markdown-points  cliaspora-regexp-user-entry 0)))
;;   (dolist (mpoint markdown-points)
;; 	(add-text-properties (cadr mpoint) (cddr mpoint)
;; 			     (list 'mouse-face 'cliaspora-mouse-highlight-face
;; 				   'face "link"
;; 				   'keymap cliaspora-show-message-map
;; 				   'cliaspora-id-message id
;; 				   'help-echo "Click here to see this message in new buffer."))
;; 	))))
    
;; (insert (propertize
;; 	 (format "%s(%s):\n" name cliaspora_id)
;; 	 'mouse-face 'cliaspora-mouse-highlight-face
;; 	 'face "link"
;; 	 'keymap cliaspora-show-message-map
;; 	 'cliaspora-id-message id
;; 	 'help-echo "Click here to see this message in new buffer."))

(defun cliaspora-show-tag-new-buffer (&rest r)
  (interactive)
  (let ((tag
         (get-text-property (+ 1 (previous-single-property-change (point) 'cliaspora-tag))
                            'cliaspora-tag)))
    (cliaspora-get-entry-stream-tag  (cliaspora-markdown-tag-strip tag))))

(defvar cliaspora-show-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cliaspora-show-tag-new-buffer)
    (define-key map [mouse-2] 'cliaspora-show-tag-new-buffer)
    map)
  "")

;; TODO
;; cliaspora-markdown-strip
;; cliaspora-html-strip

(defun cliaspora-markdown-tag-strip (string)
  (save-match-data
    (if (string-match "#\\([a-zA-Z0-9_/\.-]+\\)" string)
        (match-string 1 string)
      string)))

(defun cliaspora-html-strip-links (string)
  "Remove all HTML links from STRING."
  (replace-regexp-in-string "\\(<a .*?>\\|</a>\\)" "" string nil t))

;; Functions to extract content from json-read
;; They are, probably, done some where else...but I don't no where
;; so there you have them.


(defun cliaspora-extract-json (e a)
  (cdr (assoc e a)))

(defun cliaspora-extract-json-list (e a)
  (cond (e
         (cliaspora-extract-json-list (cdr e) 
                                      (cliaspora-extract-json (car e) a)))
        (a)))

(defun cliaspora-get-stream-by-tag (tag)
  "Get a stream of the messages with the tag given by 'tag'.
The tag must be a string without the starting \"#\"."
  (interactive "sTag(without '#')?")
  (cliaspora-get-stream-by-name (format "/tags/%s" tag)))

(defun cliaspora-tag-stream ()
  "Get the stream by the tag #cliaspora-el so you can see the latest news of cliaspora.el!"
  (interactive)
  (cliaspora-get-stream-by-tag cliaspora-tag)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cliaspora-get-url(url)
  "Get a cliaspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept-Language" . "en")
           ("Accept-Charset" . "utf-8")))
        (buffer-file-coding-system 'utf-8)
        )
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

(defsubst cliaspora-string-trim (string)
  "Remove leading and trailing whitespace and all properties from STRING.
If STRING is nil return an empty string."
  (if (null string)
      ""
    (if (string-match "\\`[ \t\n]+" string)
        (setq string (substring string (match-end 0))))
    (if (string-match "[ \t\n]+\\'" string)
        (setq string (substring string 0 (match-beginning 0))))
    (substring-no-properties string)))

(defun cliaspora-get-image-if-necessary (url)
  "If it hasn'd downloaded, download the image and save it in the temp directory."
  (let ((image-name (file-name-nondirectory url)))
    (unless (file-exists-p (cliaspora-image-path image-name))
      (cliaspora-get-image-sync url))
    (cliaspora-image-path image-name)))

(defun cliaspora-show-image-at-region ()
  "Consider the region as the image's URL, download it(if necessary) and open an external program to see it."
  (interactive)
  (let ((url (buffer-substring-no-properties (region-beginning) (region-end))))
    (cliaspora-get-image-if-necessary url)
    (cliaspora-open-image-program (cliaspora-image-path (file-name-nondirectory url)))
    )
  )

(defun cliaspora-show-image-at-point ()
  "Show only the image at the cursor."
  (interactive)
  (let ((image-url (cliaspora-get-image-link-at-point)))
    (cliaspora-get-image-if-necessary image-url)
    (cliaspora-open-image-program (cliaspora-image-path (file-name-nondirectory image-url)))))
  

(defun cliaspora-open-image-program (image-path)
  (let ((command-string (concat
                         cliaspora-image-external-program
                         " "
                         image-path)))
    (async-shell-command command-string)))

                                        ; ********************
                                        ; Authenticity token functions

(defun cliaspora-get-authenticity-token-if-necessary (&optional url get-anyway)
  "Check `cliaspora-get-always-authenticity-token', and if it's true get the authenticity token. 
If false, check if there is an authenticity token saved, if not get it.

If URL is a string then get from this URL instead from (`cliaspora-url' `cliaspora-sign-in-url')(sing in URL).
If GET-ANYWAY is t then get it from Internet despite everything.
"
  (when (or get-anyway
            cliaspora-get-always-authenticity-token 
            (null cliaspora-auth-token))
    ;; Get the authenticity token    
    (if url
        (cliaspora-authenticity-token url)
      (cliaspora-authenticity-token (cliaspora-url cliaspora-sign-in-url)))
    (message "Cliaspora: Authenticity token obtained")))

(defun cliaspora-get-last-post-time (stream-json-parsed)
  "Return the last post time so you can use it for fetching the next part of the streams with older posts.
The return value is a list in the format like `current-time' or `encode-date'(in fact I use that function):
  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent.

STREAM-JSON-PARSED is the stream in JSON format parsed with `json-read'."
  (let* (
         (post-arr stream-json-parsed);; return the posts array
         (last-post (aref post-arr (1- (length post-arr))));; return the last post
         (interacted-date (cdr (assoc 'created_at last-post)));; return the string with the last created_at date
         (year (string-to-number (substring interacted-date 0 4)))
         (month (string-to-number (substring interacted-date 5 7)))
         (day (string-to-number (substring interacted-date 8 10)))
         (hour (string-to-number (substring interacted-date 11 13)))
         (min (string-to-number (substring interacted-date 14 16)))
         (sec (string-to-number (substring interacted-date 17 19)))
         )
    (encode-time sec min hour day month year)
    )
  )



(defun cliaspora-like-message (&rest r)
  "Send a \"like\" for this message!"
  (interactive)
  (cond ((search-backward-regexp "POST-ID: \\([0-9]+\\)" nil t) 
         (cliaspora-send-likes (match-string 1)))
         ((search-forward-regexp "POST-ID: \\([0-9]+\\)" nil t)   
          (cliaspora-send-likes (match-string 1)))
         (t (message "Unable to find the post-id (located in the text property `cliaspora-id-message')."))))

(defun cliaspora-send-likes (post-id)
  "Send a like POST for the message with id given by POST-ID."
  (when post-id
    (shell-command-to-string (concat "cliaspora like " post-id))))

(provide 'cliaspora-stream)