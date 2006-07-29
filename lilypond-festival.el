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

(require 'cl)
(require 'lilypond-mode)


(defcustom LilyPond-synthesize-command "lilysong"
  "Command used to sing LilyPond files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-play-command "play"
  "Command used to play WAV files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-midi->wav-command "timidity -Ow -o"
  "Command used to make a WAV file from a MIDI file."
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

(defun LilyPond-change-language ()
  (interactive)
  (setq LilyPond-language
        (completing-read "Lyrics language: " '("en" "cs"))))

(defun LilyPond-update-language ()
  (unless LilyPond-language
    (LilyPond-change-language)))

(defun LilyPond-play-song (song)
  (call-process LilyPond-play-command nil 0 nil (LilyPond-file->wav song)))

(defstruct LilyPond-song-compilation-data
  command
  makefile
  buffer
  songs
  midi
  in-parallel)
(defvar LilyPond-song-compilation-data nil)
(defun LilyPond-sing-list (songs &optional in-parallel midi-files)
  (LilyPond-update-language)
  (add-to-list 'compilation-finish-functions 'LilyPond-song-after-compilation)
  (let* ((makefile (LilyPond-song-makefile (current-buffer) songs midi-files))
         (command (format "make -f %s" makefile)))
    (setq LilyPond-song-compilation-data
          (make-LilyPond-song-compilation-data
           :command command
           :makefile makefile
           :buffer (current-buffer)
           :songs songs
           :midi midi-files
           :in-parallel in-parallel))
    (if (LilyPond-song-up-to-date makefile)
        (LilyPond-song-handle-files LilyPond-song-compilation-data)
      (compile command))))

(defun LilyPond-song-up-to-date (makefile)
  (equal (call-process "make" nil nil nil "-f" makefile "-q") 0))

(defun LilyPond-song-makefile (buffer songs midi-files)
  (let ((temp-file (make-temp-file "Makefile.lilysong-el"))
        (language LilyPond-language))
    (with-temp-file temp-file
      (let ((master-file (save-excursion
                           (set-buffer buffer)
                           (LilyPond-get-master-file)))
            (lilyfiles (append songs midi-files)))
        (insert "all:")
        (dolist (f (mapcar 'LilyPond-file->wav lilyfiles))
          (insert " " f))
        (insert "\n")
        (when lilyfiles
          (dolist (f lilyfiles)
            (insert f " "))
          (insert ": " master-file "\n")
          (insert "\t" LilyPond-lilypond-command " " master-file "\n")
          (dolist (f songs)
            (insert (LilyPond-file->wav f) ": " f "\n")
            (insert "\t" LilyPond-synthesize-command " $< " (or language "") "\n"))
          (dolist (f midi-files)
            (insert (LilyPond-file->wav f) ": " f "\n")
            (insert "\t" LilyPond-midi->wav-command " "
                    (LilyPond-file->wav f) " " f "\n")))))
    temp-file))

(defun LilyPond-song-after-compilation (buffer message)
  (let ((data LilyPond-song-compilation-data))
    (when (and data
               (equal compile-command
                      (LilyPond-song-compilation-data-command data))
               (LilyPond-song-up-to-date (LilyPond-song-compilation-data-makefile data)))
      (LilyPond-song-handle-files data))))

(defun LilyPond-song-handle-files (data)
  (delete-file (LilyPond-song-compilation-data-makefile data))
  (setq LilyPond-last-language LilyPond-language)
  (LilyPond-sing-files (LilyPond-song-compilation-data-in-parallel data)
                       (LilyPond-song-compilation-data-songs data)
                       (LilyPond-song-compilation-data-midi data)))
  
(defun LilyPond-sing-files (in-parallel songs midi-files)
  (let ((files (mapcar 'LilyPond-file->wav (append songs midi-files))))
    (if in-parallel
        (dolist (f files)
          (start-process "lilysong-el" nil LilyPond-play-command f))
      (apply 'start-process "lilysong-el" nil LilyPond-play-command files))))

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
        (LilyPond-sing-list songs t midi-files)
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
          (LilyPond-sing-list songs t (nreverse midi-files))
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
