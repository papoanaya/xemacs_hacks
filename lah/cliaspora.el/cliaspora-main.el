;;; cliaspora-main.el --- 
;; 
;; Filename: cliaspora-main.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: vie abr 20 09:40:20 2012 (-0300)
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

(defconst cliaspora-main-buffer-name "D*"
  "This is the name of the main buffer.")

(defcustom  cliaspora-main-list-of-options 
  '("Streams";; This is a particular item
    ("Entry Stream" . cliaspora-get-entry-stream)
    ("My Activity" . cliaspora-get-activity-stream)
    ("My Stream" . cliaspora-get-my-stream)

    ("Get Tag Stream" . cliaspora-get-stream-by-tag)
    ("Get Contact Stream" . cliaspora-get-stream-by-contact)
    ("Get Stream using a Username" . cliaspora-get-stream-by-username)
    ("Get Liked Stream" . cliaspora-get-liked-stream)
    "Personal Conversations(Messages)"
    ("Messages" . cliaspora-messages)
    "Posting"
    ("Post a text" . cliaspora-post-to)
    "Configuration"
    ("Change Login" . cliaspora-login)
    ("Change POD" . cliaspora-set-pod)
    ("Customize D*.el" . cliaspora-customize))
  "This is a list of elements to show in the main menu.

Is a list of cons with the name of the option and the function to call."
  :group 'cliaspora
  :type '(alist :key-type (group string) :value-type (group function)))




    
(defun cliaspora-main ()
  "¡Show the main menu!"
  (interactive)

  (with-current-buffer (get-buffer-create cliaspora-main-buffer-name)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (cliaspora-main-insert-title)
      (cliaspora-main-insert-list-of-options)
      (goto-char (point-min))
      (cliaspora-main-mode)))
  (switch-to-buffer cliaspora-main-buffer-name))

(defface cliaspora-main-title-face 
  '((t (:weight bold :foreground "green" )))
  "This is the MAIN TITLE face."
   :group 'cliaspora-faces)

(defun cliaspora-main-insert-title ()
  "Insert a title in the current buffer."  
  (insert "Welcome to Cliaspora*.el!") 
  (center-line)
  (insert "\n\n"))

(defun cliaspora-main-insert-list-of-options ()
  "Write down all the list of options."
  (dolist (option cliaspora-main-list-of-options)
    (if (listp option)
        (insert (propertize (car option)
                            'cliaspora-main-option (car option)
                            'mouse-face 'cliaspora-mouse-highlight-face
                            'help-echo "Click or press Enter to execute the option.")
                "\n")
      (insert "\n*" option "*\n"))))


(defun cliaspora-main-next-option (&rest r)
  "Go to next option... if there is no more, goto the first one."
  (interactive)
  (cliaspora-misc-next-option 'cliaspora-main-option))

(defun cliaspora-main-execute-option (&rest r)
  "Execute the selected option."
  (interactive)
  (let ((option (get-text-property (point) 'cliaspora-main-option)))
    (if option
        (call-interactively (cdr (assoc option cliaspora-main-list-of-options))))))

(defface cliaspora-main-mode-option-face
  '((t (:inherit 'cliaspora-buttons-elements-face)))
  "Face for the options in the Main buffer."
  :group 'cliaspora-faces)

(defface cliaspora-main-mode-subtitle-face
  '((t ( :inherit 'cliaspora-header-face-2 :weight bold ) ))  
  "Face for each of the sub-titles in Main buffer."
  :group 'cliaspora-faces
  )

(defvar cliaspora-main-mode-map
  (let ((cliaspora-main-mode-map (make-sparse-keymap)))
    (define-key cliaspora-main-mode-map [return] 'cliaspora-main-execute-option)
    (define-key cliaspora-main-mode-map [mouse-2] 'cliaspora-main-execute-option)
    (define-key cliaspora-main-mode-map [tab] 'cliaspora-main-next-option)
    (define-key cliaspora-main-mode-map "q" 'cliaspora-main-exit)
    cliaspora-main-mode-map)
  )

;(defvar cliaspora-main-mode-font-lock-keywords
;  '(("[[:space:]]*Welcome.*$" . cliaspora-main-title-face)
;     ("^\*.*\*$" . cliaspora-main-mode-subtitle-face)
;     ("^[^\*[:space:]].+?$" . cliaspora-main-mode-option-face))
;  "Syntax highlighting for D*")


(defvar cliaspora-main-mode-font-lock-keywords
  '(("[[:space:]]*Welcome.*$" . cliaspora-main-title-face)
     ("^[^\*[:space:]].+?$" . cliaspora-main-mode-option-face))
  "Syntax highlighting for D*")
;;     ("^\*.*\*$" . cliaspora-main-title-face)





(define-derived-mode cliaspora-main-mode nil "D*-mode"
  "Major mode for D* main buffer."

  (set (make-local-variable 'font-lock-defaults)
       '(cliaspora-main-mode-font-lock-keywords)) 

  (use-local-map cliaspora-main-mode-map)
  (set (make-local-variable 'buffer-read-only) t))



(defun cliaspora-main-exit (&rest r)
  "Exit D* Main Window."
  (interactive) 
  (cliaspora-close-session)
  (kill-buffer cliaspora-main-buffer-name))

(provide 'cliaspora-main)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliaspora-main.el ends here