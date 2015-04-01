;;; cliaspora.el --- Simple Emacs-based client for cliaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: cliaspora*
;; URL: http://diale.org/cliaspora.html

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>


(defcustom cliaspora-regexp-pod-user
  "Regular expression for user pod and user name: 1-> user, 2-> pod."
  :type 'regexp
  :group 'cliaspora)

(defun cliaspora-get-all-regexp (regexp &optional opt)
  (save-excursion
    (cond ((search-forward-regexp regexp (point-max) t)
	   (cons (match-string-no-properties 
		  (if (not opt) 2 opt))
		 (cliaspora-get-all-regexp regexp (if (not opt) 2 opt))))
	  (t nil))))

(defun cliaspora-pod-user (str)
  (list
   (cdr (cons (string-match "\\([.a-zA-Z0-9 \\-_]+\\)" str)
	      (match-string-no-properties 0 str)))
   (cdr  (cons (string-match "@\\([.a-zA-Z0-9 \\-_-]+\\)" str)
	       (match-string-no-properties 1 str)))))

(defun cliaspora-webfinger-user-at-pod (str)
  (cliaspora-webfinger (cadr (cliaspora-pod-user str))
		      (car (cliaspora-pod-user str))))

