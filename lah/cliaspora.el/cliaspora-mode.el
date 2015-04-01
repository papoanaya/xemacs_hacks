;;; cliaspora-mode.el --- 
;; 
;; Filename: cliaspora-mode.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr  4 11:47:07 2012 (-0300)
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
;; This library provides functions and variables that create the major 
;; mode called `cliaspora-mode'.
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

(require 'font-lock)

(defgroup cliaspora-mode nil
  "`cliaspora-mode' behaviour customization."
  :group 'cliaspora
  :version "23.0"
  :tag "Cliaspora-mode")


;; Font lock

					; ********************
					; Regexps

(defgroup cliaspora-regexps nil
  "Regexp used to locate faces in `cliaspora-mode'."
  :group 'cliaspora-mode
  :version "23.0"
  :tag "Cliaspora Regexps")

;  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z"
(defcustom cliaspora-regexp-date
  "[0-9-:T]+Z"
  "Regular expression date in cliaspora stream."
  :type 'regexp
  :group 'cliaspora)

;; (defcustom cliaspora-regex-bare-link
;;   "http://[a-zA-Z0-9-_\./?=&]*"
;; or "^http://.*"
;;   "Regular expression for a `http://'"
;;   :type 'regexp
;;   :group 'cliaspora-mode)

(defcustom cliaspora-regexp-youtube-link
  "^\\(http.*://www.youtube.com/watch\\?v=\\)\\([^\)].*\\)"
  "Regular expression for a youtube link"
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-image-alist
  "\\(`?http.://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?"
  "Taken from iimage-mode."
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-image
"!\\(\\[[^]]*?\\]\\)(\\(`?http.*:[^\\)?]*\\))"
  "Regular expression for a [text](file) or an image link ![text](file).
Note: this is not correct! Needs more thought to get all images right."
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-user-entry 
"^[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*\/@]*[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*\/@]*@[a-zA-Z0-9\s-]*[\.a-zA-Z0-9\s-]*)"
  "Regular expression for user entry."
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-tag
  "#[a-zA-Z0-9_/\.-]+"
  "Regular expression for a tag."
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-header-1
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1"
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-header-2
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2"
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-header-3
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3"
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-header-4
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4"
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ]\\(.\\|\n[^\n]\\)*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments."
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text."
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-emph
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching emph text."
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses."
  :type 'regexp
  :group 'cliaspora-regexps)

(defcustom cliaspora-regexp-user-name
  "\\(\\(^\\|[[:space:]]+\\)\\(@[[:alnum:]_\.-]+\\|@{[^}]*}\\)\\)"
;;+\\|@{[[:alnum:]]+\\([[:space:]]*;[[:space:]]*[[:alnum:]]+@[^}]+}\\)?\\)"
  "Regular expression for matching the user citation in a post(like \"@myusername\")."
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines."
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching markdown horizontal rules."
  :type 'regexp
  :group 'cliaspora-regexps)


(defcustom cliaspora-regexp-buttons-elements
  "\\(Read in new buffer\\)"
  "Regular expression for matching buttons like \"Read in new buffer\".
This buttons are used by the user for clicking or pressing ENTER."
  :type 'regexp
  :group 'cliaspora-regexps)

					; ********************
					; Faces

(defgroup cliaspora-faces nil
  "Faces used in cliaspora Mode."
  :group 'cliaspora-mode
  :group 'faces
  :tag "Cliaspora Faces"
  :version "23.0")


(defface cliaspora-mouse-highlight-face 
  '((t (:weight bold
       :foreground "green"
       :background "gray20"
       :box (:line-width 2 :color "grey10" :style pressed-button))
       ))
  "Face used when mouse is over a button, link, etc."
  :group 'cliaspora-faces
  )

(defvar cliaspora-header-face-1 'cliaspora-header-face-1
  "Face name to use for level-1 headers.")

(defvar cliaspora-header-face-2 'cliaspora-header-face-2
  "Face name to use for level-2 headers.")

(defvar cliaspora-header-face-3 'cliaspora-header-face-3
  "Face name to use for level-3 headers.")

(defvar cliaspora-header-face-4 'cliaspora-header-face-4
  "Face name to use for level-4 headers.")

(defvar cliaspora-url-face 'cliaspora-url-face
  "Face name to use for URLs.")

(defvar cliaspora-link-face 'cliaspora-link-face
  "Face name to use for links.")

(defvar cliaspora-emph-face 'cliaspora-emph-face
  "Face name to use for links.")

(defvar cliaspora-bold-face 'cliaspora-bold-face
  "Face name to use for links.")

(defvar cliaspora-emph-face 'cliaspora-emph-face 
  "Face name to use for links.")

(defvar cliaspora-inline-code-face 'cliaspora-inline-code-face
  "Face name to use for inline code.")

(defvar cliaspora-blockquote-face 'cliaspora-blockquote-face
  "Face name to use for blockquote text.")

(defface cliaspora-inline-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'cliaspora-faces)

(defface cliaspora-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'cliaspora-faces)

(defface cliaspora-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'cliaspora-faces)

(defface cliaspora-header-face-1
  '((t (:inherit cliaspora-header-face :height 2.0 :underline t)))
  "Face for level-1 headers."
  :group 'cliaspora-faces)

(defface cliaspora-header-face-2
  '((t (:inherit cliaspora-header-face :height 1.4)))
  "Face for level-2 headers."
  :group 'cliaspora-faces)

(defface cliaspora-header-face-3
  '((t (:inherit cliaspora-header-face :slant italic)))
  "Face for level-3 headers."
  :group 'cliaspora-faces)

(defface cliaspora-header-face-4
  '((t (:inherit cliaspora-header-face)))
  "Face for level-4 headers."
  :group 'cliaspora-faces)

(defface cliaspora-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'cliaspora-faces)

(defface cliaspora-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'cliaspora-faces)

(defface cliaspora-tag-face
  '((t (:inherit cliaspora-url-face)))
  "Face for tags."
  :group 'cliaspora-faces)

(defface cliaspora-emph-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for italic text."
  :group 'cliaspora-faces)

(defface cliaspora-bold-face
  '((t (:inherit font-lock-variable-name-face :bold t)))
  "Face for bold text."
  :group 'cliaspora-faces)

(defface cliaspora-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'cliaspora-faces)

(defface cliaspora-date-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for date."
  :group 'cliaspora-faces)


(defface cliaspora-buttons-elements-face
  '((t (:weight bold
       :foreground "green"
       :background "grey20"
       :box (:line-width 2 :color "grey20" :style released-button)
       )))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-message-separator-face
  '((t (:foreground "black"
       :background "black"
       :box (:line-width 2 :style pressed-button :color "grey10"))
       ))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-comments-start-face
  '((t (:weight bold
       :foreground "green"
       :background "gray20"
       :box (:line-width 2 :color "grey10" :style pressed-button))
       ))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-user-name-face
  '((t (:underline t 
       :weight bold 
       :foreground "cyan2"
;;       :box (:line-width 2 :color "grey10" :style pressed-button)
       )))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-comment-user-name-face
  '((t (:inherit cliaspora-user-name-face 
       :foreground "cyan4"
       :height 0.8
       )))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-comment-text-face
  '((t (:foreground "grey50"
       :family "freeserif"
     )))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-amount-comments-face
  '((t (:inherit cliaspora-comments-start-face )))
  "Face for buttons like \"Read in new buffer\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-user-name-citation-face
  '((t (:weight bold :foreground "light sea green")))
  "Face for username's citation like \"@myname\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-unread-notification-face
  '((t (:weight bold :foreground "firebrick2")))
  "Face for username's citation like \"@myname\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-readed-notification-face
  '((t (:weight bold :foreground "forest green")))
  "Face for username's citation like \"@myname\"."
  :group 'cliaspora-faces
  )

(defface cliaspora-mark-as-unread-face
  '((t (:inherit cliaspora-buttons-elements-face)))
  "Face for username's citation like \"@myname\"."
  :group 'cliaspora-faces
  )

(defun cliaspora-check-is-property (limit property)
  "Return t if the symbol property given by PROPERTY is in any of the text's properties between current `point' up to LIMIT.
Set `match-data' with the beginning and end position of the first text founded with that property.

Create a new function like `cliaspora-check-is-message-separator' so you can use this function with a font-lock property."
  ;; Point is on a link-to-publication text!
  (let ((beg-pos (text-property-any (point) limit property t))
	(end-pos 0)
	)
    (if beg-pos	
	(progn
	  (goto-char beg-pos)
	  (setq end-pos 
		(next-single-property-change (point) property nil limit)) ;;find the last char where the property is false.    
	    
	  ;; Set match-data
	  (set-match-data (list beg-pos end-pos))
	  t
	  )
      nil
      )
    )
  )

(defun cliaspora-check-is-link-to-pub (limit)  
  "Return t if the text from the current point up to the limit has the property cliaspora-is-link-to-public setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-link-to-pub)
  )

(defun cliaspora-check-is-message-separator (limit)  
  "Return t if the text from the current point up to the limit has the property cliaspora-message-separator setted to t."
  (cliaspora-check-is-property limit 'cliaspora-message-separator)
  )

(defun cliaspora-check-is-comments-start (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-comments-start setted to t."
  (cliaspora-check-is-property limit 'cliaspora-comments-start)
  )

(defun cliaspora-check-is-user-name (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-is-user-name setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-user-name)
  )

(defun cliaspora-check-is-comment-user-name (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-is-user-name setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-comment-user-name)
  )

(defun cliaspora-check-is-amount-comments (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-comments-start setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-amount-comments)
  )

(defun cliaspora-check-is-comment-text (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-comments-start setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-comment-text)
  )

(defun cliaspora-check-is-like-link (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-is-like-link setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-like-link)
  )

(defun cliaspora-check-is-unread-notification (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-is-like-link setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-unread-notification)
  )

(defun cliaspora-check-is-readed-notification (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-is-like-link setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-readed-notification)
  )

(defun cliaspora-check-is-mark-as-unread (limit)
  "Return t if the text from the current point up to the limit has the property cliaspora-is-like-link setted to t."
  (cliaspora-check-is-property limit 'cliaspora-is-notification-mark-as-unread)
  )


(defcustom cliaspora-mode-hook '(cliaspora-see-regexp-markdow cliaspora-show-videos)
  "Functions run upon entering `cliaspora-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill longlines-mode cliaspora-get-all-images cliaspora-show-images)
  :group 'cliaspora-mode)

(defvar cliaspora-mode-font-lock-keywords
  (list
   ;; (cons cliaspora-regexp-bare-link '(2 cliaspora-url-face t))
   ;; (cons cliaspora-regexp-date 'cliaspora-date-face)    
   (cons cliaspora-regexp-blockquote ''cliaspora-blockquote-face)
   (cons cliaspora-regexp-header-1 ''cliaspora-header-face-1)
   (cons cliaspora-regexp-header-2 ''cliaspora-header-face-2)
   (cons cliaspora-regexp-header-3 ''cliaspora-header-face-3)
   (cons cliaspora-regexp-header-4 ''cliaspora-header-face-4)
   (cons cliaspora-regexp-hr ''cliaspora-header-face-1)
   (cons cliaspora-regexp-image
   	 ''((1 cliaspora-link-face t)
   	   (2 cliaspora-url-face t)))
   (cons cliaspora-regexp-user-name ''cliaspora-user-name-citation-face)
   (cons cliaspora-regexp-bold ''(2 cliaspora-bold-face))
   (cons cliaspora-regexp-emph ''(2 cliaspora-emph-face))
   (cons cliaspora-regexp-code ''(2 cliaspora-inline-code-face))
   (cons cliaspora-regexp-email ''cliaspora-link-face)
   (cons cliaspora-regexp-tag ''cliaspora-url-face)
   ;;(cons cliaspora-regexp-buttons-elements ''cliaspora-buttons-elements-face)
   (cons 'cliaspora-check-is-link-to-pub ''cliaspora-buttons-elements-face)
   (cons 'cliaspora-check-is-like-link ''cliaspora-buttons-elements-face)
   (cons 'cliaspora-check-is-message-separator ''cliaspora-message-separator-face)
   (cons 'cliaspora-check-is-comments-start ''cliaspora-comments-start-face)
   (cons 'cliaspora-check-is-user-name ''cliaspora-user-name-face)
   (cons 'cliaspora-check-is-amount-comments ''cliaspora-amount-comments-face)
   (cons 'cliaspora-check-is-comment-user-name ''cliaspora-comment-user-name-face)
   (cons 'cliaspora-check-is-comment-text ''cliaspora-comment-text-face)
   (cons 'cliaspora-check-is-unread-notification ''cliaspora-unread-notification-face)
   (cons 'cliaspora-check-is-readed-notification ''cliaspora-readed-notification-face)
   (cons 'cliaspora-check-is-mark-as-unread ''cliaspora-mark-as-unread-face)
   )   
  "Syntax highlighting for cliaspora files.")

(defvar cliaspora-mode-map 
  (let ((cliaspora-mode-map (make-sparse-keymap)))
    (define-key cliaspora-mode-map "\C-cp" 'cliaspora-post-this-buffer)
    (define-key cliaspora-mode-map "\C-c\C-cp" 'cliaspora-post-to)
    (define-key cliaspora-mode-map "\C-c\C-cc" 'cliaspora-post-clipboard)
    (define-key cliaspora-mode-map "\C-c\C-k" 'cliaspora-post-destroy)
    cliaspora-mode-map)
  "Keymap based on html-mode")

;;;###autoload
(define-derived-mode cliaspora-mode text-mode "cliaspora"
  "Major mode for output from \\[cliaspora*]."
  (set (make-local-variable 'font-lock-defaults)
       '(cliaspora-mode-font-lock-keywords))
  ;;(set (make-local-variable 'font-lock-multiline) t)
  (use-local-map cliaspora-mode-map)
  ;;(set (make-local-variable 'buffer-read-only) t)
  (run-hooks 'cliaspora-mode-hook))


(provide 'cliaspora-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-mode.el ends here
