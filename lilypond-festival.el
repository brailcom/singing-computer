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

(ignore-errors (require 'ecasound))


(defcustom LilyPond-synthesize-command "lilysong"
  "Command used to sing LilyPond files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-play-command (or (executable-find "ecaplay") "play")
  "Command used to play WAV files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-midi->wav-command "timidity -Ow -o"
  "Command used to make a WAV file from a MIDI file."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-use-ecasound (and (featurep 'ecasound)
                                      (executable-find "ecasound")
                                      t)
  "If non-nil, use ecasound for mixing and playing songs."
  :group 'LilyPond
  :type 'boolean)


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
(make-variable-buffer-local 'LilyPond-song-list-history)

(defvar LilyPond-default-songs nil)
(make-variable-buffer-local 'LilyPond-default-songs)

(defvar LilyPond-last-singing-args nil)
(make-variable-buffer-local 'LilyPond-last-singing-args)

(defun LilyPond-song-list (multi)
  (cond
   ((eq multi 'all)
    (LilyPond-all-songs))
   (multi
    (LilyPond-select-songs))
   (t
    (LilyPond-select-single-song))))

(defun LilyPond-select-single-song ()
  (let ((song (LilyPond-current-song)))
    (if song
        (list song)
      (error "No song found"))))

(defun LilyPond-select-songs ()
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

(defun LilyPond-midi-list (multi)
  (if multi
      (let ((midi-string (LilyPond-string-all-midi))
            (midi-files '()))
        (while (string-match "^\\([^ ]+\\) \\(.*\\)$" midi-string)
          (push (match-string 1 midi-string) midi-files)
          (setq midi-string (match-string 2 midi-string)))
        midi-files)
    (list (LilyPond-string-current-midi))))

(defun LilyPond-file->wav (filename &optional extension)
  (format "%s.%s" (if (string-match "\\.midi$" filename)
                      filename
                    (file-name-sans-extension filename))
          (or extension "wav")))

(defun LilyPond-file->ewf (filename)
  (LilyPond-file->wav filename "ewf"))

(defun LilyPond-change-language ()
  (interactive)
  (setq LilyPond-language
        (completing-read "Lyrics language: " '("en" "cs"))))

(defun LilyPond-update-language ()
  (unless LilyPond-language
    (LilyPond-change-language)))

(defun LilyPond-play-files (files)
  (apply 'start-process "lilysong-el" nil LilyPond-play-command files))

(defstruct LilyPond-song-compilation-data
  command
  makefile
  buffer
  songs
  midi
  in-parallel)
(defvar LilyPond-song-compilation-data nil)
(defun LilyPond-sing (songs &optional midi-files in-parallel)
  (setq LilyPond-last-singing-args (list songs midi-files in-parallel))
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
        (dolist (f (mapcar 'LilyPond-file->wav (append songs midi-files)))
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
          ;; We can't use midi files in ecasound directly, because setpos
          ;; doesn't work on them.
          (dolist (f midi-files)
            (insert (LilyPond-file->wav f) ": " f "\n")
            (insert "\t" LilyPond-midi->wav-command " "
                    (LilyPond-file->wav f) " " f "\n"))
          )))
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
  (funcall (if LilyPond-use-ecasound
               'LilyPond-sing-files-ecasound
             'LilyPond-sing-files-play)
           in-parallel songs midi-files))

(defun LilyPond-sing-files-play (in-parallel songs midi-files)
  (let ((files (mapcar 'LilyPond-file->wav (append songs midi-files))))
    (if in-parallel
        (dolist (f files)
          (LilyPond-play-files (list f)))
      (LilyPond-play-files files))))

(defun LilyPond-sing-make-ewf-files (files)
  (let ((offset 0.0))
    (dolist (f files)
      (let* ((wav-file (LilyPond-file->wav f))
             (length (with-temp-buffer
                       (call-process "ecalength" nil t nil "-s" wav-file)
                       (goto-char (point-max))
                       (forward-line -1)
                       (read (current-buffer)))))
        (with-temp-file (LilyPond-file->ewf f)
          (insert "source = " wav-file "\n")
          (insert (format "offset = %s\n" offset))
          (insert "start-position = 0.0\n")
          (insert (format "length = %s\n" length))
          (insert "looping = false\n"))
        (setq offset (+ offset length))))))

(when (and (featurep 'ecasound)
           (not (fboundp 'eci-cs-set-param)))
  (defeci cs-set-param ((parameter "sChainsetup option: " "%s"))))

(defun LilyPond-sing-files-ecasound (in-parallel songs midi-files)
  (ecasound)
  (eci-cs-add "lilysong-el")
  (eci-cs-select "lilysong-el")
  (eci-cs-remove)
  (eci-cs-add "lilysong-el")
  (eci-cs-select "lilysong-el")
  (eci-cs-set-param "-z:mixmode,sum")
  (unless in-parallel
    (LilyPond-sing-make-ewf-files songs)
    ;; MIDI files should actually start with each of the songs
    (mapc 'LilyPond-sing-make-ewf-files (mapcar 'list midi-files)))
  (let* ((file->wav (if in-parallel 'LilyPond-file->wav 'LilyPond-file->ewf))
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

(defun LilyPond-song-arg->multi (arg)
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
  (let ((multi (LilyPond-song-arg->multi arg)))
    (LilyPond-sing (LilyPond-song-list multi) '() (listp arg))))

(defun LilyPond-command-sing-and-play (&optional arg)
  "Sing lyrics and play midi of the current LilyPond buffer.
Without any prefix argument, sing and play current \festival* and \midi
commands.
With the universal prefix argument, ask which parts to sing and play.
With a double universal prefix argument, sing and play all the parts."
  (interactive "P")
  (let ((multi (LilyPond-song-arg->multi arg)))
    (LilyPond-sing (LilyPond-song-list multi) (LilyPond-midi-list multi) t)))

(defun LilyPond-command-sing-last ()
  "Repeat last LilyPond singing command."
  (interactive)
  (if LilyPond-last-singing-args
      (apply 'LilyPond-sing LilyPond-last-singing-args)
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

(provide 'lilypond-festival)


;;; lilypond-festival.el ends here
