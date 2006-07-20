;;; lilypond-festival.el --- Emacs support for LilyPond Festival singing

;; Copyright (C) 2006 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Commentary:

;;

;;; Code:

(require 'lilypond-mode)


(defcustom LilyPond-synthesize-command "lilysong"
  "Command used to sing LilyPond files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-play-command "play"
  "Command used to play WAV files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-mix-command "soxmix"
  "Command used to mix several WAV files into a single file."
  :group 'LilyPond
  :type 'string)


(defvar LilyPond-language nil)
(make-variable-buffer-local 'LilyPond-language)

(defvar LilyPond-last-language nil)
(make-variable-buffer-local 'LilyPond-last-language)


(defvar LilyPond-festival-command-regexp
  "\\\\festival\\(syl\\)? +#\"\\([^\"]+\\)\"")

(defun LilyPond-string-find-song (direction)
  "Find XML file name of the nearest Festival command in the given DIRECTION.
DIRECTION is one of the symbols `forward' or `backward'.
If no Festival command is found in the current buffer, return nil.
The point is left at the position where the command occurrence was found."
  (when (funcall (if (eq direction 'backward)
                     're-search-backward
                   're-search-forward)
                 LilyPond-festival-command-regexp nil t)
    (match-string-no-properties 2)))

(defun LilyPond-current-song ()
  "Return the XML file name corresponding to the song around current point.
If there is none, return nil."
  (save-excursion
    (or (progn (end-of-line) (LilyPond-string-find-song 'backward))
        (progn (beginning-of-line) (LilyPond-string-find-song 'forward)))))

(defun LilyPond-all-songs (&optional limit-to-region)
  "Return list of XML file names of the song commands in the current document.
If there are none, return an empty list.
If LIMIT-TO-REGION is non-nil, look for the commands in the current region
only."
  (let ((result '())
        (current nil))
    (save-excursion
      (save-restriction
        (when (or limit-to-region
                  (eq LilyPond-command-current 'LilyPond-command-region))
          (narrow-to-region (or (mark) (point)) (point)))
        (goto-char (point-min))
        (while (setq current (LilyPond-string-find-song 'forward))
          (setq result (cons current result)))))
    (nreverse result)))

(defvar LilyPond-song-list-history nil)

(defvar LilyPond-default-songs nil)

(defun LilyPond-song-list ()
  (let* ((all-songs (LilyPond-all-songs))
         (available-songs all-songs)
         (initial-songs (or LilyPond-default-songs (LilyPond-all-songs t)))
         (last-input (completing-read
                      (format "Sing file%s: "
                              (if initial-songs
                                  (format " (default `%s')"
                                          (mapconcat 'identity initial-songs
                                                     ", "))
                                ""))
                      all-songs
                      nil t nil
                      'LilyPond-song-list-history)))
    (if (equal last-input "")
        initial-songs
      (let ((song-list '())
            default-input)
        (while (not (equal last-input ""))
          (push last-input song-list)
          (setq default-input (second (member last-input available-songs)))
          (setq available-songs (remove last-input available-songs))
          (setq last-input (completing-read "Sing file: "
                                            available-songs
                                            nil t default-input
                                            'LilyPond-song-list-history)))
        (setq LilyPond-default-songs (nreverse song-list))))))

(defun LilyPond-file->wav (filename)
  (format "%s.wav" (file-name-sans-extension filename)))

(defun LilyPond-song-update-needed (source results)
  (let ((update-needed nil))
    (while (and (not update-needed)
                results)
      (if (file-newer-than-file-p source (car results))
          (setq update-needed t)
        (setq results (cdr results))))
    update-needed))

(defun LilyPond-change-language ()
  (interactive)
  (setq LilyPond-language
        (completing-read "Lyrics language: " '("en" "cs"))))

(defun LilyPond-update-language ()
  (unless LilyPond-language
    (LilyPond-change-language)))

(defun LilyPond-play-song (song)
  (call-process LilyPond-play-command nil 0 nil (LilyPond-file->wav song)))

(defun LilyPond-sing-list (songs &optional parallel midi-files)
  (LilyPond-update-language)
  (let ((commands nil)
        (update-needed (or (LilyPond-song-update-needed
                            (buffer-file-name) songs)
                           (LilyPond-song-update-needed
                            (LilyPond-get-master-file) songs)
                           (LilyPond-song-update-needed
                            (buffer-file-name) midi-files)
                           (LilyPond-song-update-needed
                            (LilyPond-get-master-file) midi-files))))
    (when update-needed
      (push (format "%s %s" LilyPond-lilypond-command
                    (LilyPond-get-master-file))
            commands))
    (mapc #'(lambda (song)
              (let ((wav-file (LilyPond-file->wav song)))
                (when (or update-needed
                          (file-newer-than-file-p song wav-file)
                          (not (equal LilyPond-language
                                      LilyPond-last-language)))
                  (push (format "%s %s %s"
                                LilyPond-synthesize-command
                                song
                                (or LilyPond-language ""))
                        commands))))
          songs)
    (mapc #'(lambda (midi)
              (let ((wav-file (LilyPond-file->wav midi)))
                (when (or update-needed
                          (file-newer-than-file-p midi wav-file))
                  (push (format "%s -Ow -o %s %s"
                                LilyPond-midi-command wav-file midi)
                        commands))))
          midi-files)
    (let ((wav-files (mapcar 'LilyPond-file->wav (append songs midi-files))))
      (if (and (eq parallel 'mix) (not midi-files) (cdr wav-files))
          (push (format "%s %s -t wav - | %s -t wav -"
                        LilyPond-mix-command
                        (mapconcat 'identity wav-files " ")
                        LilyPond-play-command)
                commands)
        (let ((play-commands (mapcar #'(lambda (wav)
                                         (format "%s %s"
                                                 LilyPond-play-command
                                                 wav))
                                     wav-files)))
          (if parallel
              (push (mapconcat 'identity play-commands " & ") commands)
            (setq commands (append (nreverse play-commands) commands))))))
    (compile (mapconcat 'identity (nreverse commands) " && ")))
  (setq LilyPond-last-language LilyPond-language))

(defun LilyPond-command-sing-current ()
  "Sing song around the current point."
  (interactive)
  (let ((song (LilyPond-current-song)))
    (if song
        (LilyPond-sing-list (list song))
      (error "No song found"))))

(defun LilyPond-command-sing-all ()
  "Sing all songs of the current buffer."
  (interactive)
  (let ((songs (LilyPond-all-songs)))
    (if songs
        (LilyPond-sing-list songs)
      (error "No song found in the current buffer"))))

(defun LilyPond-command-sing-parallel (&optional midi-files)
  "Play selected songs in parallel.
The optional MIDI-FILES argument is a list of midi file names to play together
with singing."
  (interactive)
  (let ((songs (LilyPond-song-list)))
    (if songs
        (LilyPond-sing-list songs 'mix midi-files)
      (error "No songs given"))))

(defun LilyPond-command-sing-play-current-midi ()
  "Sing selected songs together with playing current midi."
  (interactive)
  (LilyPond-command-sing-parallel (list (LilyPond-string-current-midi))))

(defun LilyPond-command-sing-play-all-midi ()
  "Sing all songs together with playing all midi."
  (interactive)
  (let ((midi-string (LilyPond-string-all-midi))
        (midi-files '()))
    (while (string-match "^\\([^ ]+\\) \\(.*\\)$" midi-string)
      (push (match-string 1 midi-string) midi-files)
      (setq midi-string (match-string 2 midi-string)))
    (let ((songs (LilyPond-all-songs)))
      (if songs
          (LilyPond-sing-list songs 'mix (nreverse midi-files))
        (error "No songs found")))))

(defun LilyPond-command-sing (&optional all)
  "Sing song arround the current point.
If invoked with a universal argument, sing all songs of the current buffer.
If invoked with a zero prefix argument, sing selected songs in parallel.
If invoked with a negative prefix argument, sing selected songs together with
playing current midi file.
If invoked with a positive prefix argument, sing all songs together with
playing all midi files."
  (interactive "P")
  (cond
   ((not all)
    (LilyPond-command-sing-current))
   ((listp all)
    (LilyPond-command-sing-all))
   ((numberp all)
    (cond
     ((= all 0)
      (LilyPond-command-sing-parallel))
     ((< all 0)
      (LilyPond-command-sing-play-current-midi))
     ((> all 0)
      (LilyPond-command-sing-play-all-midi))))))

(define-key LilyPond-mode-map "\C-c\C-a" 'LilyPond-command-sing)

(easy-menu-add-item LilyPond-command-menu nil
  ["Sing" LilyPond-command-sing-current t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Parallel" LilyPond-command-sing-parallel t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing All" LilyPond-command-sing-all t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play" LilyPond-command-sing-play-current-midi t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play All" LilyPond-command-sing-play-all-midi t])


;;; Announce

(provide 'lilypond-festival)


;;; lilypond-festival.el ends here
