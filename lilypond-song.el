;;; lilypond-song.el --- Emacs support for LilyPond singing

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

;; This file adds Emacs support for singing lyrics of LilyPond files.
;; It extends lilypond-mode with the following commands (see their
;; documentation for more information):
;; 
;; - M-x LilyPond-command-sing (C-c C-a)
;; - M-x LilyPond-command-sing-and-play
;; - M-x LilyPond-command-sing-last (C-c C-z)
;; 
;; Note these commands are not available from the standard LilyPond mode
;; command menus.

;;; Code:


(eval-when-compile (require 'cl))
(require 'lilypond-mode)

(ignore-errors (require 'ecasound))


;;; User options


(defcustom LilyPond-synthesize-command "lilysong"
  "Command used to sing LilyPond files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-play-command (or (executable-find "ecaplay") "play")
  "Command used to play WAV files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-midi->wav-command "timidity -Ow -o %t %s"
  "Command used to make a WAV file from a MIDI file.
%s in the string is replaced with the source MIDI file name,
%t is replaced with the target WAV file name."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-use-ecasound (and (featurep 'ecasound)
                                      (executable-find "ecasound")
                                      t)
  "If non-nil, use ecasound for mixing and playing songs."
  :group 'LilyPond
  :type 'boolean)


;;; Lyrics language handling


(defvar lilysong-language nil)
(make-variable-buffer-local 'lilysong-language)

(defvar lilysong-last-language nil)
(make-variable-buffer-local 'lilysong-last-language)

(defun lilysong-change-language ()
  (interactive)
  (setq lilysong-language
        (completing-read "Lyrics language: " '("en" "cs"))))

(defun lilysong-update-language ()
  (unless lilysong-language
    (lilysong-change-language)))


;;; Looking for \festival* and \midi commands


(defvar lilysong-festival-command-regexp
  "\\\\festival\\(syl\\)? +#\"\\([^\"]+\\)\"")

(defun lilysong-find-song (direction)
  "Find XML file name of the nearest Festival command in the given DIRECTION.
DIRECTION is one of the symbols `forward' or `backward'.
If no Festival command is found in the current buffer, return nil.
The point is left at the position where the command occurrence was found."
  (save-match-data
    (when (funcall (if (eq direction 'backward)
                       're-search-backward
                     're-search-forward)
                   lilysong-festival-command-regexp nil t)
      (match-string-no-properties 2))))

(defun lilysong-current-song ()
  "Return the XML file name corresponding to the song around current point.
If there is none, return nil."
  (save-excursion
    (or (progn (end-of-line) (lilysong-find-song 'backward))
        (progn (beginning-of-line) (lilysong-find-song 'forward)))))

(defun lilysong-all-songs (&optional limit-to-region)
  "Return list of XML file names of the song commands in the current document.
If there are none, return an empty list.
If LIMIT-TO-REGION is non-nil, look for the commands in the current region
only."
  (let ((result '())
        (current nil))
    (save-excursion
      (save-restriction
        (when limit-to-region
          (narrow-to-region (or (mark) (point)) (point)))
        (goto-char (point-min))
        (while (setq current (lilysong-find-song 'forward))
          (push current result))))
    (nreverse result)))

(defvar lilysong-song-history nil)
(make-variable-buffer-local 'lilysong-song-history)

(defvar lilysong-last-song-list nil)
(make-variable-buffer-local 'lilysong-last-song-list)

(defvar lilysong-last-command-args nil)
(make-variable-buffer-local 'lilysong-last-command-args)

(defun lilysong-song-list (multi)
  (cond
   ((eq multi 'all)
    (lilysong-all-songs))
   (multi
    (lilysong-select-songs))
   (t
    (lilysong-select-single-song))))

(defun lilysong-select-single-song ()
  (let ((song (lilysong-current-song)))
    (if song
        (list song)
      (error "No song found"))))

(defun lilysong-select-songs ()
  (let* ((all-songs (lilysong-all-songs))
         (available-songs all-songs)
         (initial-songs (if (or (not lilysong-last-song-list)
                                (eq LilyPond-command-current
                                    'LilyPond-command-region))
                            (lilysong-all-songs t)
                          lilysong-last-song-list))
         (last-input (completing-read
                      (format "Sing file%s: "
                              (if initial-songs
                                  (format " (default `%s')"
                                          (mapconcat 'identity initial-songs
                                                     ", "))
                                ""))
                      all-songs
                      nil t nil
                      'lilysong-song-history)))
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
                                            'lilysong-song-history)))
        (setq lilysong-last-song-list (nreverse song-list))))))

(defun lilysong-midi-list (multi)
  (if multi
      (let ((midi-string (LilyPond-string-all-midi))
            (midi-files '()))
        (save-match-data
          (while (string-match "^\\([^ ]+\\) \\(.*\\)$" midi-string)
            (push (match-string 1 midi-string) midi-files)
            (setq midi-string (match-string 2 midi-string))))
        midi-files)
    (list (LilyPond-string-current-midi))))


;;; Compilation


(defun lilysong-file->wav (filename &optional extension)
  (format "%s.%s" (save-match-data
                    (if (string-match "\\.midi$" filename)
                        filename
                      (file-name-sans-extension filename)))
          (or extension "wav")))

(defun lilysong-file->ewf (filename)
  (lilysong-file->wav filename "ewf"))

(defstruct lilysong-compilation-data
  command
  makefile
  buffer
  songs
  midi
  in-parallel)
(defvar lilysong-compilation-data nil)
(defun lilysong-sing (songs &optional midi-files in-parallel)
  (setq lilysong-last-command-args (list songs midi-files in-parallel))
  (lilysong-update-language)
  (add-to-list 'compilation-finish-functions 'lilysong-after-compilation)
  (let* ((makefile (lilysong-makefile (current-buffer) songs midi-files))
         (command (format "make -f %s" makefile)))
    (setq lilysong-compilation-data
          (make-lilysong-compilation-data
           :command command
           :makefile makefile
           :buffer (current-buffer)
           :songs songs
           :midi midi-files
           :in-parallel in-parallel))
    (if (lilysong-up-to-date-p makefile)
        (lilysong-process-generated-files lilysong-compilation-data)
      (compile command))))

(defun lilysong-up-to-date-p (makefile)
  (equal (call-process "make" nil nil nil "-f" makefile "-q") 0))

(defun lilysong-makefile (buffer songs midi-files)
  (let ((temp-file (make-temp-file "Makefile.lilysong-el"))
        (language lilysong-language))
    (with-temp-file temp-file
      (let ((master-file (save-excursion
                           (set-buffer buffer)
                           (LilyPond-get-master-file)))
            (lilyfiles (append songs midi-files)))
        (insert "all:")
        (dolist (f (mapcar 'lilysong-file->wav (append songs midi-files)))
          (insert " " f))
        (insert "\n")
        (when lilyfiles
          (dolist (f lilyfiles)
            (insert f " "))
          (insert ": " master-file "\n")
          (insert "\t" LilyPond-lilypond-command " " master-file "\n")
          (dolist (f songs)
            (insert (lilysong-file->wav f) ": " f "\n")
            (insert "\t" LilyPond-synthesize-command " $< " (or language "") "\n"))
          ;; We can't use midi files in ecasound directly, because setpos
          ;; doesn't work on them.
          (dolist (f midi-files)
            (insert (lilysong-file->wav f) ": " f "\n")
            (let ((command LilyPond-midi->wav-command))
              (when (string-match "%s" command)
                (setq command (replace-match f nil nil command)))
              (when (string-match "%t" command)
                (setq command (replace-match (lilysong-file->wav f) nil nil command)))
              (insert "\t" command "\n")))
          )))
    temp-file))

(defun lilysong-after-compilation (buffer message)
  (let ((data lilysong-compilation-data))
    (when (and data
               (equal compile-command
                      (lilysong-compilation-data-command data))
               (lilysong-up-to-date-p (lilysong-compilation-data-makefile data)))
      (lilysong-process-generated-files data))))

(defun lilysong-process-generated-files (data)
  (delete-file (lilysong-compilation-data-makefile data))
  (setq lilysong-last-language lilysong-language)
  (lilysong-play-files (lilysong-compilation-data-in-parallel data)
                       (lilysong-compilation-data-songs data)
                       (lilysong-compilation-data-midi data)))


;;; Playing files


(defun lilysong-play-files (in-parallel songs midi-files)
  (funcall (if LilyPond-use-ecasound
               'lilysong-play-with-ecasound
             'lilysong-play-with-play)
           in-parallel songs midi-files))

(defun lilysong-call-play (files)
  (apply 'start-process "lilysong-el" nil LilyPond-play-command files))

(defun lilysong-play-with-play (in-parallel songs midi-files)
  (let ((files (mapcar 'lilysong-file->wav (append songs midi-files))))
    (if in-parallel
        (dolist (f files)
          (lilysong-call-play (list f)))
      (lilysong-call-play files))))

(defun lilysong-make-ewf-files (files)
  (let ((offset 0.0))
    (dolist (f files)
      (let* ((wav-file (lilysong-file->wav f))
             (length (with-temp-buffer
                       (call-process "ecalength" nil t nil "-s" wav-file)
                       (goto-char (point-max))
                       (forward-line -1)
                       (read (current-buffer)))))
        (with-temp-file (lilysong-file->ewf f)
          (insert "source = " wav-file "\n")
          (insert (format "offset = %s\n" offset))
          (insert "start-position = 0.0\n")
          (insert (format "length = %s\n" length))
          (insert "looping = false\n"))
        (setq offset (+ offset length))))))

(when (and (featurep 'ecasound)
           (not (fboundp 'eci-cs-set-param)))
  (defeci cs-set-param ((parameter "sChainsetup option: " "%s"))))

(defun lilysong-play-with-ecasound (in-parallel songs midi-files)
  (ecasound)
  (eci-cs-add "lilysong-el")
  (eci-cs-select "lilysong-el")
  (eci-cs-remove)
  (eci-cs-add "lilysong-el")
  (eci-cs-select "lilysong-el")
  (eci-cs-set-param "-z:mixmode,sum")
  (unless in-parallel
    (lilysong-make-ewf-files songs)
    ;; MIDI files should actually start with each of the songs
    (mapc 'lilysong-make-ewf-files (mapcar 'list midi-files)))
  (let* ((file->wav (if in-parallel 'lilysong-file->wav 'lilysong-file->ewf))
         (files (mapcar file->wav (append songs midi-files))))
    (dolist (f files)
      (eci-c-add f)
      (eci-c-select f)
      (eci-ai-add f))
    (eci-c-select-all)
    (eci-ao-add-default)
    (let* ((n (length songs))
           (right (if (<= n 1) 50 0))
           (step (if (<= n 1) 0 (/ 100.0 (1- n)))))
      (dolist (f songs)
        (let ((chain (funcall file->wav f)))
          (eci-c-select chain)
          (eci-cop-add "-erc:1,2")
          (eci-cop-add (format "-epp:%f" (min right 100)))
          (incf right step))))
    (eci-start)))


;;; User commands


(defun lilysong-arg->multi (arg)
  (cond
   ((not arg)
    nil)
   ((equal arg '(4))
    t)
   (t
    'all)))
     
(defun LilyPond-command-sing (&optional arg)
  "Sing lyrics of the current LilyPond buffer.
Without any prefix argument, sing current \festival* command.
With the universal prefix argument, ask which parts to sing.
With a double universal prefix argument, sing all the parts.
With a numeric prefix argument, ask which parts to sing and sing them
sequentially rather than in parallel."
  (interactive "P")
  (let ((multi (lilysong-arg->multi arg)))
    (lilysong-sing (lilysong-song-list multi) '() (listp arg))))

(defun LilyPond-command-sing-and-play (&optional arg)
  "Sing lyrics and play midi of the current LilyPond buffer.
Without any prefix argument, sing and play current \festival* and \midi
commands.
With the universal prefix argument, ask which parts to sing and play.
With a double universal prefix argument, sing and play all the parts."
  (interactive "P")
  (let ((multi (lilysong-arg->multi arg)))
    (lilysong-sing (lilysong-song-list multi) (lilysong-midi-list multi) t)))

(defun LilyPond-command-sing-last ()
  "Repeat last LilyPond singing command."
  (interactive)
  (if lilysong-last-command-args
      (apply 'lilysong-sing lilysong-last-command-args)
    (error "No previous singing command")))

(define-key LilyPond-mode-map "\C-c\C-a" 'LilyPond-command-sing)
(define-key LilyPond-mode-map "\C-c\C-z" 'LilyPond-command-sing-last)

(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Current" LilyPond-command-sing t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Selected" (LilyPond-command-sing '(4)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing All" (LilyPond-command-sing '(16)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Selected Sequentially" (LilyPond-command-sing 1) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play Current" LilyPond-command-sing-and-play t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play Selected" (LilyPond-command-sing-and-play '(4)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play All" (LilyPond-command-sing-and-play '(16)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Last" LilyPond-command-sing-last t])


;;; Announce

(provide 'lilypond-song)


;;; lilypond-song.el ends here