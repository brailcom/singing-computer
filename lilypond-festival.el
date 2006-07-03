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
The cursor is left at the position where the command occurrence was found."
  (when (funcall (if (eq direction 'backward)
                     're-search-backward
                   're-search-forward)
                 LilyPond-festival-command-regexp nil t)
    (match-string 2)))

(defun LilyPond-current-song ()
  "Return the XML file name corresponding to the song around current point.
If there is none, return nil."
  (save-excursion
    (or (LilyPond-string-find-song 'backward)
        (LilyPond-string-find-song 'forward))))

(defun LilyPond-all-songs ()
  "Return XML file names corresponding to the songs of the current document.
If there are none, return an empty string."
  (let ((result '())
        (current nil))
    (save-excursion
      (save-restriction
        (when (eq LilyPond-command-current 'LilyPond-command-region)
          (narrow-to-region (mark) (point)))
        (goto-char (point-min))
        (while (setq current (LilyPond-string-find-song 'forward))
          (setq result (cons current result)))))
    (nreverse result)))

(defun LilyPond-xml->wav (filename)
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
  (call-process LilyPond-play-command nil 0 nil (LilyPond-xml->wav song)))

(defun LilyPond-command-sing-list (songs)
  (LilyPond-update-language)
  (if (or (LilyPond-song-update-needed (buffer-file-name) songs)
          (LilyPond-song-update-needed (LilyPond-get-master-file) songs))
      ;; We must call both LilyPond processing and synthesis here to ensure
      ;; sequential execution of these actions.
      (compile
       (format "%s %s && for f in %s; do %s -p %s $f %s; done"
               LilyPond-lilypond-command
               (LilyPond-get-master-file)
               (mapconcat 'identity songs " ")
               LilyPond-synthesize-command
               LilyPond-play-command
               LilyPond-language))
    (compile (mapconcat #'(lambda (song)
                            (let ((wav-file (LilyPond-xml->wav song)))
                              (if (or (file-newer-than-file-p song wav-file)
                                      (not (equal LilyPond-language LilyPond-last-language)))
                                  (format "%s -p %s %s %s"
                                          LilyPond-synthesize-command
                                          LilyPond-play-command
                                          song
                                          (or LilyPond-language ""))
                                (format "%s %s" LilyPond-play-command wav-file))))
                        songs
                        "; ")))
  (setq LilyPond-last-language LilyPond-language))

(defun LilyPond-command-sing-current ()
  "Sing song around the current point."
  (interactive)
  (let ((song (LilyPond-current-song)))
    (if song
        (LilyPond-command-sing-list (list song))
      (error "No song found"))))

(defun LilyPond-command-sing-all ()
  "Sing all songs of the current buffer."
  (interactive)
  (LilyPond-command-sing-list (LilyPond-all-songs)))

(defun LilyPond-command-sing (&optional all)
  "Sing song arround the current point.
If invoked with a prefix argument, sing all songs of the current buffer."
  (interactive "P")
  (if all
      (LilyPond-command-sing-all)
    (LilyPond-command-sing-current)))


(define-key LilyPond-mode-map "\C-c\C-a" 'LilyPond-command-sing)

(easy-menu-add-item LilyPond-command-menu nil
  ["Sing" LilyPond-command-sing-current t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing All" LilyPond-command-sing-all t])


;;; Announce

(provide 'lilypond-festival)


;;; lilypond-festival.el ends here
