;;; mpd.el --- A complete ripoff of xwem-mpd.

;; Copyright (C) 2008 Steve Youngs

;; Original xwem-mpd:
;; Copyright (C) 2005 Richard Klinda
;; Author: Richard Klinda <ignotus@freemail.hu>
;;         Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 2004

;; Keywords: music, entertainment

;; This file is NOT part of anything.

;; The original xwem-mpd.el was released under the terms of the GPLv2.
;; mpd.el uses the BSD licence.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES# LOSS OF USE, DATA, OR PROFITS# OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; You need MusicPD - Music Playing Daemon
;; (http://musicpd.sourceforge.net/) to be set up and running.

;;; Code:

(eval-when-compile
  (autoload 'google-query "google-query" nil t))

(defgroup mpd nil
  "Group to customize mpd."
  :prefix "mpd-"
  :group 'hypermedia)

(defcustom mpd-update-rate 5
  "MPD variables updating rate in seconds."
  :type 'number
  :group 'mpd)

(defcustom mpd-directory
  (file-name-as-directory
   (expand-file-name ".mpd" (user-home-directory)))
  "Base mpd directory."
  :type 'directory
  :group 'mpd)

(defcustom mpd-lyrics-dir
  (file-name-as-directory
   (expand-file-name "lyrics" mpd-directory))
  "Directory containing songs lyrics."
  :type 'directory
  :group 'mpd)

(defcustom mpd-after-command-hook nil
  "Hooks to run after MPD command is executed.
Executed command name stored in `mpd-this-command'."
  :type 'hook
  :group 'mpd)

(defcustom mpd-before-variables-update-hook nil
  "Hooks to run before updating mpd variables."
  :type 'hook
  :group 'mpd)

(defcustom mpd-after-variables-update-hook nil
  "Hooks to run after mpd variables has been updated."
  :type 'hook
  :group 'mpd)


(defvar mpd-process nil)
(defvar mpd-itimer nil)
(defvar mpd-dock-frame nil)
(defvar mpd-dock-buffer nil)

(defun mpd-start-connection ()
  "Open connection to MusicPD daemon.
Set `mpd-process' by side effect."
  (when (or (not mpd-process)
            (not (eq mpd-process 'open)))
    (setq mpd-process (open-network-stream "mpd" " *mpd connection*"
                                           "localhost" 6600))
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system mpd-process 'utf-8 'utf-8))
    (set-process-filter mpd-process 'mpd-process-filter)
    (set-process-sentinel mpd-process 'mpd-process-sentinel)
    (process-kill-without-query mpd-process)

    (add-hook 'mpd-after-command-hook #'mpd-update-variables)
    (setq mpd-itimer
          (start-itimer "mpd-vars-update" #'mpd-update-variables
                        mpd-update-rate mpd-update-rate))))

(defun mpd-disconnect ()
  "Disconnect from the mpd daemon.

Also removes the update hook, kills the itimer, and removes the dock
frame."
  (interactive)
  (let ((proc (get-process (process-name mpd-process)))
	(timer (get-itimer (itimer-name mpd-itimer))))
    (remove-hook 'mpd-after-command-hook #'mpd-update-variables)
    (when (itimerp timer)
      (delete-itimer timer))
    (when (process-live-p proc)
      (delete-process (get-process (process-name mpd-process))))
    (when (frame-live-p mpd-dock-frame)
      (delete-frame mpd-dock-frame))
    (when (buffer-live-p mpd-dock-buffer)
      (kill-buffer mpd-dock-buffer))))

;; mpd variables
(defvar mpd-zero-vars-p t)
(defvar mpd-status-update-p nil)

(defvar **mpd-var-Album* nil)
(defvar **mpd-var-Artist* nil)
(defvar **mpd-var-Date* nil)
(defvar **mpd-var-Genre* nil)
(defvar **mpd-var-Id* nil)
(defvar **mpd-var-Pos* nil)
(defvar **mpd-var-Time* nil)
(defvar **mpd-var-Title* nil)
(defvar **mpd-var-Track* nil)
(defvar **mpd-var-audio* nil)
(defvar **mpd-var-bitrate* nil)
(defvar **mpd-var-file* nil)
(defvar **mpd-var-length* nil)
(defvar **mpd-var-playlist* nil)
(defvar **mpd-var-playlistlength* nil)
(defvar **mpd-var-random* nil)
(defvar **mpd-var-repeat* nil)
(defvar **mpd-var-song* nil)
(defvar **mpd-var-songid* nil)
(defvar **mpd-var-state* nil)
(defvar **mpd-var-time* nil)
(defvar **mpd-var-volume* nil)
(defvar **mpd-var-xfade* nil)

(defvar mpd-pre-mute-volume nil
  "Holds the value of `**mpd-var-volume* prior to muting.
The purpose of this is so that when you unmute, it goes back to the
volume you had it set to before you muted.")

(defvar mpd-this-command nil
  "The mpd command currently executing.
Useful to use in `mpd-after-command-hook' hooks.")

(defmacro define-mpd-command (cmd args &rest body)
  "Define new mpd command."
  `(defun ,cmd ,args
     ,@body
     (let ((mpd-this-command ',cmd))
       (run-hooks 'mpd-after-command-hook))))

(defun mpd-send (format &rest args)
  "Send formated string into connection.
FORMAT and ARGS are passed directly to `format' as arguments."
  (let ((string (concat (apply #'format format args) "\n")))
    (if (eq (process-status mpd-process) 'open)
	(process-send-string mpd-process string)
      (mpd-start-connection)
      (process-send-string mpd-process string))))

(defun mpd-stopped-p ()
  (string= **mpd-var-state* "stop"))
(defun mpd-paused-p ()
  (string= **mpd-var-state* "pause"))
(defun mpd-muted-p ()
  (zerop (string-to-number **mpd-var-volume*)))

;; (mpd-songpos)
(defun mpd-songpos ()
  (if **mpd-var-time*
      (destructuring-bind (a b)
          (split-string **mpd-var-time* ":")
        (cons (string-to-int a) (string-to-int b)))
    (cons 0 1)))                        ; todo?

(defun mpd-volume-up (step)
  "Increase the volume by STEP increments.
STEP can be given via numeric prefix arg and defaults to 1 if omitted."
  (interactive "p")
  (let* ((oldvol (string-to-number **mpd-var-volume*))
	 (newvol (+ oldvol step))
	 (mpd-this-command 'mpd-volume-down))
    (when (>= newvol 100)
      (setq newvol 100))
    (mpd-send "setvol %d" newvol)
    (run-hooks 'mpd-after-command-hook)))

(defun mpd-volume-down (step)
  "Decrease the volume by STEP increments.
STEP can be given via numeric prefix arg and defaults to 1 if omitted."
  (interactive "p")
  (let* ((oldvol (string-to-number **mpd-var-volume*))
	 (newvol (- oldvol step))
	 (mpd-this-command 'mpd-volume-down))
    (when (<= newvol 0)
      (setq newvol 0))
    (mpd-send "setvol %d" newvol)
    (run-hooks 'mpd-after-command-hook)))

(defun mpd-volume-mute (&optional unmute)
  "Mute the volume.
With prefix arg, UNMUTE, let the tunes blast again."
  (interactive "P")
  (if unmute
      (mpd-send "setvol %s" mpd-pre-mute-volume)
    (setq mpd-pre-mute-volume **mpd-var-volume*)
    (mpd-send "setvol 0"))
  (let ((mpd-this-command 'mpd-volume-mute))
    (run-hooks 'mpd-after-command-hook)))

(defun mpd-volume-mute/unmute ()
  "Wrapper around #'mpd-volume-mute to mute and unmute."
  (interactive)
  (if (mpd-muted-p)
      (mpd-volume-mute 'unmute)
    (mpd-volume-mute)))

(define-mpd-command mpd-volume-max ()
  "Set volume to maximum."
  (interactive)
  (mpd-send "setvol 100"))

(define-mpd-command mpd-volume-min ()
  "Set volume to minimum.
Sets state to \"muted\" by side effect."
  (interactive)
  (setq mpd-pre-mute-volume **mpd-var-volume*)
  (mpd-send "setvol 0"))

(define-mpd-command mpd-seek (time)
  "Seek current track to TIME."
  (mpd-send "seekid %s %d" **mpd-var-Id* (+ (car (mpd-songpos)) time)))

(defun mpd-seek-forward ()
  (interactive)
  (mpd-seek 10))

(defun mpd-seek-backward ()
  (interactive)
  (mpd-seek -10))

;; Plaing operations
(define-mpd-command mpd-next-track ()
  "Start playing next track."
  (interactive)
  (mpd-send "next"))

(define-mpd-command mpd-previous-track ()
  "Start playing previous track."
  (interactive)
  (mpd-send "previous"))

(define-mpd-command mpd-stop ()
  "Stop playing."
  (interactive)
  (mpd-send "stop"))

(define-mpd-command mpd-play ()
  "Start playing."
  (interactive)
  (mpd-send "play"))

(define-mpd-command mpd-pause ()
  "Pause playing."
  (interactive)
  (mpd-send "pause"))

(define-mpd-command mpd-playpause ()
  "Resume playing or pause."
  (interactive)
  (if (mpd-stopped-p)
      (mpd-send "play")
    (mpd-send "pause")))

(defun mpd-process-filter (process output)
  "MPD proccess filter."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "\\(.*?\\): \\(.*\\)")
        (set (intern (format "**mpd-var-%s*" (match-string 1)))
             (match-string 2)))
      (forward-line 1)))
  (when mpd-status-update-p
    (setq mpd-status-update-p nil)
    (setq mpd-zero-vars-p nil)
    (run-hooks 'mpd-after-variables-update-hook)))

(defun mpd-process-sentinel (proc &optional evstr)
  (let ((timer (get-itimer mpd-itimer)))
    (message "[MPD]: %s" evstr)
    (delete-process proc)
    (when (itimerp timer)
      (delete-itimer timer))
    (setq mpd-itimer nil
	  mpd-process nil)))

(defun mpd-update-variables ()
  "Requests status information."
  (run-hooks 'mpd-before-variables-update-hook)
  (setq mpd-zero-vars-p t)
  (mpd-send "currentsong")
  (setq mpd-status-update-p t)
  (mpd-send "status"))

;;; Lyrics support
(defun mpd-lyric-filename ()
  "Return lyric filename for now playing song."
  (when **mpd-var-file*
    (expand-file-name
     (concat (replace-in-string **mpd-var-file* "\/" "--") ".txt")
     mpd-lyrics-dir)))

(defun mpd-lyric-check ()
  "Return non-nil if current track has local lyrics."
  (let ((fn (mpd-lyric-filename)))
    (and fn (file-exists-p fn))))

(defun mpd-lyric-save ()
  "Save selected lyric to lyric file."
  (interactive "_")
  (if (mpd-lyric-check)
      (message "There is already a lyric for this song")
    (let ((text (get-selection-no-error)))
      (if (not text)
          (message "You should have selected the lyric first!")
        ;; everything is ok
        (with-current-buffer (find-file-noselect (mpd-lyric-filename))
          (insert text)
          (save-buffer))))))

;; Haven't decided what to do with this one yet. --SY.
;;(define-sawfish-command mpd-lyric-show ()
;;  "Show lyrics for now playing song."
;;  (sawfish-interactive)
;;  (if (mpd-lyric-check)
;;      (let ((temp-buffer-show-function 'sawfish-special-popup-frame)
;;	    (header (format "\"%s\" (by: %s)"
;;			    **mpd-var-Title*
;;			    **mpd-var-Artist*))
;;	    (title (format "Lyrics: %s" **mpd-var-Title*)))
;;	(with-output-to-temp-buffer title
;;	  (set-buffer standard-output)
;;	  (insert header "\n"
;;		  (make-string (length header) ?=)
;;		  "\n\n")
;;	  (insert-file-contents (mpd-lyric-filename))
;;	  (toggle-read-only 1)
;;          (view-mode nil #'(lambda (&rest not-used-buffer)
;;                             (delete-frame (selected-frame))))))
;;    (when (and **mpd-var-Artist* **mpd-var-Title*)
;;      (let ((lyric-frame (new-frame)))
;;	(select-frame lyric-frame)
;;	(google-query (format "\"%s\" \"%s\" lyrics"
;;			      **mpd-var-Artist* **mpd-var-Title*))
;;	(focus-frame lyric-frame)))))


;;;; Dockapp section
(defvar mpd-dock-frame-plist
  '((name . "MpdDock")
    (height . 4)
    (width . 12)
    (unsplittable . t)
    (minibuffer . none)
    (menubar-visible-p . nil)
    (has-modeline-p . nil)
    (default-gutter-visible-p . nil)
    (default-toolbar-visible-p . nil)
    (scrollbar-height . 0)
    (scrollbar-width . 0)
    (text-cursor-visible-p . nil))
  "Frame properties for mpd dock.")

(defun mpd-info (&rest args)
  (let ((title (or **mpd-var-Title* "Unknown"))
	(artist (or **mpd-var-Artist* "Unknown"))
	(album (or **mpd-var-Album* "Unknown"))
	(genre (or **mpd-var-Genre* "Unknown"))
	(year (or **mpd-var-Date* "Unknown"))
	(file (file-name-nondirectory **mpd-var-file*)))
    (format "--[ %s ]\n
Artist: %s
Album: %s
Year: %s  Genre: %s\n
--[ %s ]"
	    title artist album year genre file)))

(defconst mpd-prev-map
  (let* ((map (make-sparse-keymap 'mpd-prev-map)))
    (define-key map [button1] 'mpd-previous-track)
    map)
  "Keymap for \"Prev\" button.")

(defconst mpd-pause-map
  (let* ((map (make-sparse-keymap 'mpd-pause-map)))
    (define-key map [button1] 'mpd-pause)
    map)
  "Keymap for \"Pause\" button.")

(defconst mpd-play-map
  (let* ((map (make-sparse-keymap 'mpd-play-map)))
    (define-key map [button1] 'mpd-play)
    map)
  "Keymap for \"Play\" button.")

(defconst mpd-next-map
  (let* ((map (make-sparse-keymap 'mpd-next-map)))
    (define-key map [button1] 'mpd-next-track)
    map)
  "Keymap for \"Next\" button.")

(make-face 'mpd-dock-face
	   "Face used in the mpd dock buffer.")

(defun mpd-new-frame ()
  "Create new mpd frame."
  (unless (frame-live-p mpd-dock-frame)
    (setq mpd-dock-frame (new-frame mpd-dock-frame-plist))
    (select-frame mpd-dock-frame)
    (unless (buffer-live-p mpd-dock-buffer)
      (setq mpd-dock-buffer (get-buffer-create "*MpdDock*"))
      (set-buffer-dedicated-frame mpd-dock-buffer mpd-dock-frame)
      (save-excursion
	(let (prev pause play next)
	  (set-buffer mpd-dock-buffer)
	  (set-extent-properties
	   (insert-face "[Song Info]" 'mpd-dock-face)
	   `(mouse-face highlight read-only t
			balloon-help ,#'mpd-info))
	  (insert "\n\n ")
	  (set-extent-end-glyph
	   (setq prev (make-extent (point-max) (point-max)))
	   (make-glyph
	    (list (vector 'xpm :file (expand-file-name "Rewind.xpm"
						       mpd-directory)))))
	  (set-extent-properties 
	   prev
	   `(keymap ,mpd-prev-map balloon-help "Previous Track"))
	  (set-extent-end-glyph
	   (setq pause (make-extent (point-max) (point-max)))
	   (make-glyph
	    (list (vector 'xpm :file (expand-file-name "Pause.xpm"
						       mpd-directory)))))
	  (set-extent-properties
	   pause
	   `(keymap ,mpd-pause-map balloon-help "Pause"))
	  (set-extent-end-glyph
	   (setq play (make-extent (point-max) (point-max)))
	   (make-glyph
	    (list (vector 'xpm :file (expand-file-name "Play.xpm"
						       mpd-directory)))))
	  (set-extent-properties
	   play
	   `(keymap ,mpd-play-map balloon-help "Play"))
	  (set-extent-end-glyph
	   (setq next (make-extent (point-max) (point-max)))
	   (make-glyph
	    (list (vector 'xpm :file (expand-file-name "FFwd.xpm"
						       mpd-directory)))))
	  (set-extent-properties
	   next
	   `(keymap ,mpd-next-map balloon-help "Next Track")))))
    (set-specifier horizontal-scrollbar-visible-p nil
		   (cons mpd-dock-frame nil))
    (set-specifier vertical-scrollbar-visible-p nil
		   (cons mpd-dock-frame nil))
    (set-window-buffer nil mpd-dock-buffer)))

(defun mpd ()
  "Start mpd dockapp to interact with MusicPD."
  (interactive)
  (let ((cframe (selected-frame)))
    ;; Start client connection
    (mpd-start-connection)
    (mpd-new-frame)
    (focus-frame cframe)
    (mpd-update-variables)))


(provide 'mpd)

;;; mpd.el ends here
