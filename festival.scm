;;; festival.scm --- Festival singing mode output

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


(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 receive))


;;; Configuration


;; The word to be sung in places where notes are played without lyrics.
;; If it is #f, the places without lyrics are omitted on the output.
(define song:*skip-word* "-skip-")

;; If true, use syllables in the Festival XML file.
;; If false, use whole words instead; this is necessary in languages like
;; English, were the phonetic form cannot be deduced from syllables well enough.
(define song:*syllabify* #f)

;; Base Festival octave to which LilyPond notes are mapped.
(define song:*base-octave* 5)
;; The resulting base octave is sum of song:*base-octave* and
;; song:*base-octave-shift*.  This is done to work around a Festival bug
;; causing Festival to segfault or produce invalid pitch on higher pitches.
;(define song:*base-octave-shift* -2)
(define song:*base-octave-shift* 0)

;; Iff true, enable a lot of debugging output
(define song:*debug* #f)


;;; LilyPond interface


(define-public (song:output-file music tempo filename)
  (if song:*debug*
      (debug-enable 'backtrace))
  (ly:message "Writing Festival XML file ~a..." filename)
  (let ((port (open-output-file filename)))
    (song:write-header port tempo)
    (song:write-lyrics port music)
    (song:write-footer port))
  #f)


;;; Debugging utilities


(define-macro (song:assert condition . data)
  (if song:*debug*
      `(if (not ,condition)
           (error "Assertion failed" (quote ,condition) ,@data))
      #f))

(define-macro (song:debug message object)
  (if song:*debug*
      `(song:debug* ,message ,object)
      object))

(define (song:debug* message object)
  (display "[[") (display message) (display "]] ") (pretty-print object)
  object)


;;; General utilities


(define-macro (song:defstruct name . slots)
  ;; Similar as in Common Lisp, but much simplier -- no structure and slot options, no docstring
  (let* ((slots* (map (lambda (s) (if (pair? s) s (list s))) slots))
         (make-symbol (lambda (format% . extra-args)
                        (string->symbol (apply format #f format% name extra-args))))
         ($record? (make-symbol "song:~a?"))
         ($make-record (make-symbol "song:make-~a"))
         ($copy-record (make-symbol "song:copy-~a"))
         (reader-format "song:~a-~a")
         (writer-format "song:set-~a-~a!")
         (record (gensym)))
    `(begin
       (define ,$record? #f)
       (define ,$make-record #f)
       (define ,$copy-record #f)
       ,@(map (lambda (s) `(define ,(make-symbol reader-format (car s)) #f)) slots*)
       ,@(map (lambda (s) `(define ,(make-symbol writer-format (car s)) #f)) slots*)
       (let ((,record ,(make-record-type name (map car slots*))))
         (set! ,$record?
               (lambda (record) ((record-predicate ,record) record)))
         (set! ,$make-record
               (lambda* (#:key ,@slots)
                 ((record-constructor ,record) ,@(map car slots*))))
         (set! ,$copy-record
               (lambda (record)
                 (,$make-record ,@(apply
                                   append
                                   (map (lambda (slot)
                                          (list (symbol->keyword slot)
                                                (list (make-symbol reader-format slot) 'record)))
                                        (map car slots*))))))
         ,@(map (lambda (s)
                  `(set! ,(make-symbol reader-format (car s))
                         (record-accessor ,record (quote ,(car s)))))
                slots*)
         ,@(map (lambda (s)
                  `(set! ,(make-symbol writer-format (car s))
                         (record-modifier ,record (quote ,(car s)))))
                slots*)))))

(define (song:compose . functions)
  (let ((functions* (drop-right functions 1))
        (last-function (last functions)))
    (letrec ((reduce (lambda (x functions)
                       (if (null? functions)
                           x
                           (reduce ((car functions) x) (cdr functions))))))
      (lambda args (reduce (apply (last functions) args) (reverse functions*))))))

(define-macro (song:push! object list-var)
  ;; The same as in Common Lisp
  `(set! ,list-var (cons ,object ,list-var)))

(define-macro (song:add! object list-var)
  `(set! ,list-var (append ,list-var (list ,object))))

(define (song:flatten lst)
  (cond
   ((null? lst)
    lst)
   ((pair? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst))))
   (else
    (cons (car lst) (flatten (cdr lst))))))

(define (song:car list)
  (if (null? list)
      #f
      (car list)))

(define (song:last list)
  (if (null? list)
      #f
      (last list)))


;;; LilyPond utility functions


(define song:pp-pitch-names '((0 . "c") (1 . "des") (2 . "d") (3 . "es") (4 . "e") (5 . "f")
                              (6 . "ges") (7 . "g") (8 . "as") (9 . "a") (10 . "bes") (11 . "b")))
(define (song:pp object)
  (cond
   ((list? object)
    (format #f "[~{~a ~}]" (map song:pp object)))
   ((song:skip? object)
    (format #f "skip(~a)" (song:skip-duration object)))
   ((song:lyrics? object)
    (format #f "~a(~a)~a" (song:lyrics-text object) (song:lyrics-duration object)
            (if (song:lyrics-unfinished object) "-" "")))
   ((song:note? object)
    (let ((pitch (ly:pitch-semitones (song:note-pitch object))))
      (format #f "~a~a~a~a"
              (cdr (assoc (modulo pitch 12) song:pp-pitch-names))
              (let ((octave (+ (inexact->exact (floor (/ pitch 12))) 1)))
                (cond
                 ((= octave 0)
                  "")
                 ((> octave 0)
                  (make-uniform-array #\' octave))
                 ((< octave 0)
                  (make-uniform-array #\, (- 0 octave)))))
              (song:pp-duration (song:note-duration object))
              (if (song:note-joined object) "-" ""))))
   ((song:rest? object)
    (format #f "r~a" (song:pp-duration (song:rest-duration object))))
   (else
    object)))

(define (song:pp-duration duration)
  (set! duration (/ 4 duration))
  (if (< (abs (- duration (inexact->exact duration))) 0.0001)
      (inexact->exact duration)
      (/ (round (* duration 100)) 100)))

(define (song:warning object-with-origin message . args)
  (let ((origin (cond
                 ((not object-with-origin)
                  #f)
                 ((song:note? object-with-origin)
                  (song:note-origin object-with-origin))
                 ((song:rest? object-with-origin)
                  (song:rest-origin object-with-origin))
                 ((ly:location? object-with-origin)
                  object-with-origin)
                 (else
                  (format #t "Minor programming error: ~a~%" object-with-origin)
                  #f))))
    (if origin
        (ly:input-message origin "***Song Warning***")
        (format #t "~%***Song Warning***"))
    (apply ly:message message (map song:pp args))))

(define (song:music-property-value? music property value)
  "Return true iff MUSIC's PROPERTY is equal to VALUE."
  (equal? (ly:music-property music property) value))

(define (song:music-name? music name)
  "Return true iff MUSIC's name is NAME."
  (if (list? name)
      (member (ly:music-property music 'name) name)
      (song:music-property-value? music 'name name)))

(define (song:music-property? music property)
  "Return true iff MUSIC is a property setter and sets or unsets PROPERTY."
  (and (song:music-name? music '(PropertySet PropertyUnset))
       (song:music-property-value? music 'symbol property)))

(define (song:music-has-property? music property)
  "Return true iff MUSIC contains PROPERTY."
  (not (eq? (ly:music-property music property) '())))

(define (song:property-value music)
  "Return value of a property setter MUSIC.
If it unsets the property, return #f."
  (if (song:music-name? music 'PropertyUnset)
      #f
      (ly:music-property music 'value)))

(define (song:music-elements music)
  "Return list of all MUSIC's top-level children."
  (let ((elt (ly:music-property music 'element))
        (elts (ly:music-property music 'elements)))
    (if (not (null? elt))
        (cons elt elts)
        elts)))

(define (song:find-child music predicate)
  "Find the first node in MUSIC that satisfies PREDICATE."
  (define (find-child queue)
    (if (null? queue)
        #f
        (let ((elt (car queue)))
          (if (predicate elt)
              elt
              (find-child (append (song:music-elements elt) (cdr queue)))))))
  (find-child (list music)))

(define (song:find-child-named music name)
  "Return the first child in MUSIC that is named NAME."
  (song:find-child music (lambda (elt) (song:music-name? elt name))))

(define (song:process-music music function)
  "Process all nodes of MUSIC (including MUSIC) in the DFS order.
Apply FUNCTION on each of the nodes.
If FUNCTION applied on a node returns true, don't process the node's subtree."
  (define (process-music queue)
    (if (not (null? queue))
        (let* ((elt (car queue))
               (stop (function elt)))
          (process-music (if stop
                             (cdr queue)
                             (append (song:music-elements elt) (cdr queue)))))))
  (process-music (list music)))


;;; Analysis functions


(define (song:duration->number duration)
  (let* ((log (ly:duration-log duration))
         (dots (ly:duration-dot-count duration))
         (factor (ly:duration-factor duration)))
    (* (expt 2 (- log)) (+ 1 (/ dots 2)) (/ (car factor) (cdr factor)))))

(define (song:tempo->beats music)
  (let ((tempo (song:find-child-named music 'MetronomeChangeEvent)))
    (if tempo
        (let* ((count (ly:music-property tempo 'metronome-count))
               (duration (ly:music-property tempo 'tempo-unit)))
          (round (* count (* (song:duration->number duration)
                             (expt 2 (+ 2 song:*base-octave-shift*)))))))))

(song:defstruct music-context
  music
  context)

(define (song:collect-lyrics-music music)
  ;; Returns list of music-context instances.
  (let ((music-context-list '()))
    (song:process-music
     music
     (lambda (music*)
       (cond
        ((song:music-name? music* 'LyricCombineMusic)
         (song:push! (song:make-music-context #:music music*
                                              #:context (ly:music-property music* 'associated-context))
                     music-context-list)
         #t)
        ((and (song:music-name? music* 'ContextSpeccedMusic)
              (song:music-property-value? music* 'context-type 'Lyrics)
              (not (song:find-child-named music* 'LyricCombineMusic)))
         (let ((name-node (song:find-child music* (lambda (node) (song:music-property? node 'associatedVoice)))))
           (if name-node
               (song:push! (song:make-music-context #:music music* #:context (song:property-value name-node))
                           music-context-list)))
         #t)
        (else
         #f))))
    (song:debug "Lyrics contexts" (reverse music-context-list))))

(song:defstruct lyrics
  text
  duration
  unfinished
  ignore-melismata
  context)

(song:defstruct skip
  duration
  context)

(define (song:get-lyrics music context)
  ;; Returns list of lyrics and skip instances.
  (let ((lyrics-list '())
        (next-ignore-melismata #f)
        (ignore-melismata #f)
        (next-current-voice context)
        (current-voice context))
    (song:process-music
     music
     (lambda (music)
       (cond
        ;; true lyrics
        ((song:music-name? music 'EventChord)
         (let ((lyric-event (song:find-child-named music 'LyricEvent)))
           (song:push! (song:make-lyrics
                        #:text (ly:music-property lyric-event 'text)
                        #:duration (* (song:duration->number (ly:music-property lyric-event 'duration)) 4)
                        #:unfinished (and (not song:*syllabify*) (song:find-child-named music 'HyphenEvent))
                        #:ignore-melismata ignore-melismata
                        #:context current-voice)
                       lyrics-list))
         ;; LilyPond delays applying settings
         (set! ignore-melismata next-ignore-melismata)
         (set! current-voice next-current-voice)
         #t)
        ;; skipping
        ((song:music-name? music 'SkipMusic)
         (song:push! (song:make-skip
                      #:duration (* (song:duration->number (ly:music-property music 'duration)) 4)
                      #:context current-voice)
                     lyrics-list)
         #t)
        ;; parameter change
        ((song:music-property? music 'ignoreMelismata)
         (set! next-ignore-melismata (song:property-value music))
         #t)
        ((song:music-property? music 'associatedVoice)
         (set! next-current-voice (song:property-value music))
         #t)
        ;; anything else
        (else
         #f))))
    (song:debug "Raw lyrics" (reverse lyrics-list))))

(song:defstruct score-voice
  context
  elements ; list of score-* instances
  )

(song:defstruct score-choice
  lists ; of lists of score-* instances
  (n-assigned 0) ; number of lists having a verse-block
  )

(song:defstruct score-repetice
  count ; number of repetitions
  elements ; list of score-* instances
  )

(song:defstruct score-notes
  note/rest-list ; list of note and rest instances
  (verse-block-list '()) ; lyrics attached to notes -- multiple elements are
                         ; possible for multiple stanzas
  )

(song:defstruct note
  pitch
  duration
  joined ; to the next note
  origin
  )
  
(song:defstruct rest
  duration
  origin
  )

(define (song:get-notes music)
  ;; Returns list of score-* instances.
  (song:get-notes* music #t))

(define (song:get-notes* music autobeaming*)
  ;; Returns list of score-* instances.
  (let* ((result-list '())
         (in-slur 0)
         (autobeaming autobeaming*))
    (song:process-music
     music
     (lambda (music)
       (cond
        ;; context change
        ((song:music-has-property? music 'context-id)
         (let ((context (ly:music-property music 'context-id))
               (children (song:music-elements music)))
           (song:add! (song:make-score-voice #:context (song:debug "Changing context" context)
                                             #:elements (append-map (lambda (elt)
                                                                      (song:get-notes* elt autobeaming))
                                                                    children))
                      result-list))
         #t)
        ;; timing change
        ((song:music-property? music 'timeSignatureFraction)
         (let ((value (song:property-value music)))
           (song:debug "Timing change" value)))
        ;; simultaneous notes
        ((song:music-name? music 'SimultaneousMusic)
         (let ((simultaneous-lists (map (lambda (child)
                                          (song:get-notes* child autobeaming))
                                        (ly:music-property music 'elements))))
           (song:debug "Simultaneous lists" simultaneous-lists)
           (song:add! (song:make-score-choice #:lists simultaneous-lists) result-list))
         #t)
        ;; repetice
        ((song:music-name? music 'VoltaRepeatedMusic)
         (let ((repeat-count (ly:music-property music 'repeat-count))
               (children (song:music-elements music)))
           (song:add! (song:make-score-repetice #:count repeat-count
                                                #:elements (append-map
                                                            (lambda (elt) (song:get-notes* elt autobeaming))
                                                            children))
                      result-list))
         #t)
        ;; a note or rest
        ((song:music-name? music 'EventChord)
         (song:debug "Simple music event" music)
         (let ((note (song:find-child-named music 'NoteEvent))
               (rest (song:find-child-named music 'RestEvent)))
           (cond
            (note
             (song:debug "Note" note)
             (let* ((pitch (ly:music-property note 'pitch))
                    (duration (* (song:duration->number (ly:music-property note 'duration)) 4))
                    (events (filter identity (list
                                              (song:find-child-named music 'SlurEvent)
                                              (song:find-child-named music 'ManualMelismaEvent)
                                              (and (not autobeaming)
                                                   (song:find-child-named music 'BeamEvent)))))
                    (slur-start (length (filter (lambda (e) (song:music-property-value? e 'span-direction -1))
                                                events)))
                    (slur-end (length (filter (lambda (e) (song:music-property-value? e 'span-direction 1))
                                              events))))
               (set! in-slur (+ in-slur slur-start (- slur-end)))
               (if (< in-slur 0)
                   (song:warning note "Slur underrun"))
               (let ((note-spec (song:make-note #:pitch pitch #:duration duration #:joined (> in-slur 0)
                                                #:origin (ly:music-property note 'origin)))
                     (last-result (and (not (null? result-list)) (last result-list))))
                 (if (and last-result
                          (song:score-notes? last-result))
                     (song:set-score-notes-note/rest-list!
                      last-result
                      (append (song:score-notes-note/rest-list last-result) (list note-spec)))
                     (song:add! (song:make-score-notes #:note/rest-list (list note-spec)) result-list)))))
            (rest
             (song:debug "Rest" rest)
             (let* ((duration (* (song:duration->number (ly:music-property rest 'duration)) 4))
                    (rest-spec (song:make-rest #:duration duration
                                               #:origin (ly:music-property rest 'origin)))
                    (last-result (and (not (null? result-list)) (last result-list))))
               (if (and last-result
                        (song:score-notes? last-result))
                   (song:set-score-notes-note/rest-list! last-result
                                                         (append (song:score-notes-note/rest-list last-result)
                                                                 (list rest-spec)))
                   (song:add! (song:make-score-notes #:note/rest-list (list rest-spec)) result-list))))))
         #f)
        ;; autobeaming change
        ((song:music-property? music 'autoBeaming)
         (set! autobeaming (song:property-value music))
         #t)
        ;; anything else
        (else
         #f))))
    (song:debug "Raw notes" result-list)))

(song:defstruct verse-block ; lyrics for a given piece of music
  verse-list
  (fresh #t) ; if #t, this block hasn't been yet included in the final output
  )

(song:defstruct parallel-blocks ; several parallel blocks (e.g. stanzas)
  block-list ; list of verse-blocks
  )

(song:defstruct sequential-blocks
  block-list ; list of verse-blocks
  )

(song:defstruct repeated-blocks
  block-list ; list of verse-blocks
  count ; number of repetitions
  )

(song:defstruct verse ; 
  text ; separate text element (syllable or word)
  notelist/rests ; list of note lists (slurs) and rests
  (unfinished #f) ; whether to be merged with the following verse
  )

(define (song:find-lyrics-score score-list context accept-default)
  ;; Returns score-* element of context or #f (if there's no such any).
  (and (not (null? score-list))
       (or (song:find-lyrics-score* (car score-list) context accept-default)
           (song:find-lyrics-score (cdr score-list) context accept-default))))

(define (song:find-lyrics-score* score context accept-default)
  (cond
   ((and (song:score-voice? score)
         (equal? (song:score-voice-context score) context))
    score)
   ((song:score-voice? score)
    (song:find-lyrics-score (song:score-voice-elements score) context #f))
   ((song:score-choice? score)
    (letrec ((lookup (lambda (lists)
                       (if (null? lists)
                           #f
                           (or (song:find-lyrics-score (car lists) context accept-default)
                               (lookup (cdr lists)))))))
      (lookup (song:score-choice-lists score))))
   ((song:score-repetice? score)
    (if accept-default
        score
        (song:find-lyrics-score (song:score-repetice-elements score) context accept-default)))
   ((song:score-notes? score)
    (if accept-default
        score
        #f))
   (else
    (error "Unknown score element" score))))

(define (song:insert-lyrics! lyrics/skip-list score-list context)
  ;; Add verse-block-lists to score-list.
  ;; Each processed score-notes instance must receive at most one block in each
  ;; song:insert-lyrics! call.  (It can get other blocks if more pieces of
  ;; lyrics are attached to the same score part.)
  (let ((lyrics-score-list (song:find-lyrics-score score-list context #f)))
    (song:debug "Lyrics+skip list" lyrics/skip-list)
    (song:debug "Corresponding score-* list" score-list)
    (if lyrics-score-list
        (song:insert-lyrics*! lyrics/skip-list (list lyrics-score-list) context)
        (song:warning #f "Lyrics context not found: ~a" context))))

(define (song:insert-lyrics*! lyrics/skip-list score-list context)
  (song:debug "Processing lyrics" lyrics/skip-list)
  (song:debug "Processing score" score-list)
  (cond
   ((and (null? lyrics/skip-list)
         (null? score-list))
    #f)
   ((null? lyrics/skip-list)
    (song:warning #f "Extra notes: ~a ~a" context score-list))
   ((null? score-list)
    (song:warning #f "Extra lyrics: ~a ~a" context lyrics/skip-list))
   (else
    (let* ((lyrics/skip (car lyrics/skip-list))
           (lyrics-context ((if (song:lyrics? lyrics/skip) song:lyrics-context song:skip-context) lyrics/skip))
           (score (car score-list)))
      (cond
       ((song:score-voice? score)
        (let ((new-context (song:score-voice-context score)))
          (if (equal? new-context lyrics-context)
              (song:insert-lyrics*! lyrics/skip-list
                                    (append (song:score-voice-elements score)
                                            (if (null? (cdr score-list))
                                                '()
                                                (list (song:make-score-voice #:context context
                                                                             #:elements (cdr score-list)))))
                                    new-context)
              (song:insert-lyrics*! lyrics/skip-list (cdr score-list) context))))
       ((song:score-choice? score)
        (let* ((lists* (song:score-choice-lists score))
               (lists lists*)
               (n-assigned (song:score-choice-n-assigned score))
               (n 0)
               (allow-default #f)
               (score* #f))
          (while (and (not score*)
                      (not (null? lists)))
            (set! score* (song:find-lyrics-score (car lists) lyrics-context allow-default))
            (set! lists (cdr lists))
            (if (not score*)
                (set! n (+ n 1)))
            (if (and (null? lists)
                     (not allow-default)
                     (equal? lyrics-context context))
                (begin
                  (set! allow-default #t)
                  (set! n 0)
                  (set! lists (song:score-choice-lists score)))))
          (song:debug "Selected score" score*)
          (if (and score*
                   (>= n n-assigned))
              (begin
                (if (> n n-assigned)
                    (receive (assigned-elts unassigned-elts) (split-at lists* n-assigned)
                      (song:set-score-choice-lists! score (append assigned-elts
                                                                  (list (list-ref lists* n))
                                                                  (take unassigned-elts (- n n-assigned))
                                                                  lists))))
                (song:set-score-choice-n-assigned! score (+ n-assigned 1))))
          (song:insert-lyrics*! lyrics/skip-list (append (if score* (list score*) '()) (cdr score-list)) context)))
       ((song:score-repetice? score)
        (song:insert-lyrics*! lyrics/skip-list
                              (append (song:score-repetice-elements score) (cdr score-list)) context))
       ((song:score-notes? score)
        ;; This is the only part which actually attaches the processed lyrics.
        ;; The subsequent calls return verses which we collect into a verse block.
        ;; We add the block to the score element.
        (if (equal? lyrics-context context)
            (set! lyrics/skip-list (song:really-insert-lyrics! lyrics/skip-list score context)))
        (song:insert-lyrics*! lyrics/skip-list (cdr score-list) context))
       (else
        (error "Unknown score element in lyrics processing" score)))))))

(define (song:really-insert-lyrics! lyrics/skip-list score context)
  ;; Return new lyrics/skip-list.
  ;; Score is modified by side effect.
  (song:debug "Assigning notes" score)
  (let ((note-list (song:score-notes-note/rest-list score))
        (unfinished-verse #f)
        (verse-list '()))
    (while (not (null? note-list))
      (if (null? lyrics/skip-list)
          (begin
            (while (and (not (null? note-list))
                        (song:rest? (car note-list)))
              (set! note-list (cdr note-list)))
            (if (not (null? note-list))
                (begin
                  (song:warning (car note-list) "Missing lyrics: ~a ~a" context note-list)
                  (set! note-list '()))))
          (let ((lyrics/skip (car lyrics/skip-list)))
            (receive (notelist/rest note-list*) (if (song:lyrics? lyrics/skip)
                                                    (song:consume-lyrics-notes lyrics/skip note-list context)
                                                    (song:consume-skip-notes lyrics/skip note-list context))
              (song:debug "Consumed notes" (list lyrics/skip notelist/rest))
              (set! note-list note-list*)
              (cond
               ((null? notelist/rest)
                #f)
               ;; Lyrics
               ((and (song:lyrics? lyrics/skip)
                     unfinished-verse)
                (song:set-verse-text!
                 unfinished-verse
                 (string-append (song:verse-text unfinished-verse) (song:lyrics-text lyrics/skip)))
                (song:set-verse-notelist/rests!
                 unfinished-verse
                 (append (song:verse-notelist/rests unfinished-verse) (list notelist/rest)))
                (if (not (song:lyrics-unfinished lyrics/skip))
                    (set! unfinished-verse #f)))
               ((song:lyrics? lyrics/skip)
                (let ((verse (song:make-verse #:text (if (song:rest? notelist/rest)
                                                         ""
                                                         (song:lyrics-text lyrics/skip))
                                              #:notelist/rests (list notelist/rest))))
                  (song:add! verse verse-list)
                  (set! unfinished-verse (if (song:lyrics-unfinished lyrics/skip) verse #f))))
               ;; Skip
               ((song:skip? lyrics/skip)
                (cond
                 ((song:rest? notelist/rest)
                  (if (null? verse-list)
                      (set! verse-list (list (song:make-verse #:text ""
                                                              #:notelist/rests (list notelist/rest))))
                      (let ((last-verse (last verse-list)))
                        (song:set-verse-notelist/rests!
                         last-verse
                         (append (song:verse-notelist/rests last-verse) (list notelist/rest))))))
                 ((pair? notelist/rest)
                  (song:add! (song:make-verse #:text song:*skip-word* #:notelist/rests (list notelist/rest))
                             verse-list))
                 (else
                  (error "Unreachable branch reached")))
                (set! unfinished-verse #f)))
              (if (not (song:rest? notelist/rest))
                  (set! lyrics/skip-list (cdr lyrics/skip-list)))))))
    (if unfinished-verse
        (song:set-verse-unfinished! unfinished-verse #t))
    (song:set-score-notes-verse-block-list!
     score
     (append (song:score-notes-verse-block-list score)
             (list (song:make-verse-block #:verse-list verse-list)))))
  lyrics/skip-list)

(define (song:consume-lyrics-notes lyrics note-list context)
  ;; Returns list of note instances + new note-list.
  (song:assert (song:lyrics? lyrics))
  (if (and (not (null? note-list))
           (song:rest? (car note-list)))
      (values (car note-list) (cdr note-list))
      (let ((ignore-melismata (song:lyrics-ignore-melismata lyrics))
            (join #t)
            (consumed '()))
        (while (and join
                    (not (null? note-list)))
          (let ((note (car note-list)))
            (song:push! note consumed)
            (set! join (and (not ignore-melismata) (song:note-joined note))))
          (set! note-list (cdr note-list)))
        (if join
            (song:warning (song:car (if (null? note-list) consumed note-list))
                          "Unfinished slur: ~a ~a" context consumed))
        (values (reverse consumed) note-list))))
  
(define (song:consume-skip-notes skip note-list context)
  ;; Returns either note list (skip word defined) or rest instance (no skip word) + new note-list.
  (song:assert (song:skip? skip))
  (let ((duration (song:skip-duration skip))
        (epsilon 0.001)
        (consumed '()))
    (while (and (> duration epsilon)
                (not (null? note-list)))
      (let ((note (car note-list)))
        (song:assert (song:note? note))
        (song:push! note consumed)
        (set! duration (- duration (song:note-duration note))))
      (set! note-list (cdr note-list)))
    (set! consumed (reverse! consumed))
    (cond
     ((> duration epsilon)
      (song:warning (if (null? note-list) (song:last consumed) (song:car note-list))
                    "Excessive skip: ~a ~a ~a ~a" context skip duration consumed))
     ((< duration (- epsilon))
      (song:warning (if (null? note-list) (song:last consumed) (song:car note-list))
                    "Skip misalignment: ~a ~a ~a ~a" context skip duration consumed)))
    (values (if song:*skip-word*
                consumed
                '())
            note-list)))

(define (song:extract-verse-blocks score)
  ;; Returns list of blocks and parallel blocks.
  (song:debug "Extracting verse blocks" score)
  (cond
   ((song:score-voice? score)
    (append-map song:extract-verse-blocks (song:score-voice-elements score)))
   ((song:score-choice? score)
    (list (song:make-parallel-blocks
           #:block-list (map (lambda (block-list)
                               (song:make-sequential-blocks
                                #:block-list (append-map song:extract-verse-blocks block-list)))
                             (song:score-choice-lists score)))))
   ((song:score-repetice? score)
    (list (song:make-repeated-blocks #:count (song:score-repetice-count score)
                                     #:block-list (append-map song:extract-verse-blocks
                                                              (song:score-repetice-elements score)))))
   ((song:score-notes? score)
    (list (song:make-parallel-blocks #:block-list (song:score-notes-verse-block-list score))))
   (else
    (error "Invalid score element" score))))

(define (song:extract-verses score-list)
  ;; Returns (final) list of verses.
  ;; The primary purpose of this routine is to build complete stanzas from
  ;; lists of verse blocks.
  ;; Extract verse-blocks and process them until no unprocessed stanzas remain.
  (song:debug "Final score list" score-list)
  (let ((verse-block-list (song:debug "Verse blocks" (append-map song:extract-verse-blocks score-list))))
    (letrec ((combine (lambda (lst-1 lst-2)
                         (song:debug "Combining lists" (list lst-1 lst-2))
                         (if (null? lst-2)
                             lst-1
                             (let ((diff (- (length lst-1) (length lst-2))))
                               (if (< diff 0)
                                   (let ((last-elt (last lst-1)))
                                     (while (< diff 0)
                                       (song:add! last-elt lst-1)
                                       (set! diff (+ diff 1))))
                                   (let ((last-elt (last lst-2)))
                                     (while (> diff 0)
                                       (song:add! last-elt lst-2)
                                       (set! diff (- diff 1)))))
                               (song:debug "Combined" (map append lst-1 lst-2))))))
             (expand* (lambda (block)
                        (cond
                         ((song:parallel-blocks? block)
                          (append-map (lambda (block) (expand (list block)))
                                      (song:parallel-blocks-block-list block)))
                         ((song:sequential-blocks? block)
                          (expand (song:sequential-blocks-block-list block)))
                         ((song:repeated-blocks? block)
                          ;; Only simple repetice without nested parallel sections is supported.
                          (let ((count (song:repeated-blocks-count block))
                                (expanded (expand (song:repeated-blocks-block-list block)))
                                (expanded* '()))
                            (while (not (null? expanded))
                              (let ((count* count)
                                    (item '()))
                                (while (and (> count* 0) (not (null? expanded)))
                                  (set! item (append item (car expanded)))
                                  (set! expanded (cdr expanded))
                                  (set! count* (- count* 1)))
                                (song:push! item expanded*)))
                            (reverse expanded*)))
                         (else
                          (list (list block))))))
             (expand (lambda (block-list)
                       (song:debug "Expanding list" block-list)
                       (if (null? block-list)
                           '()
                           (song:debug "Expanded" (combine (expand* (car block-list))
                                                           (expand (cdr block-list)))))))
             (merge (lambda (verse-list)
                      (cond
                       ((null? verse-list)
                        '())
                       ((song:verse-unfinished (car verse-list))
                        (let ((verse-1 (first verse-list))
                              (verse-2 (second verse-list)))
                          (merge (cons (song:make-verse #:text (string-append (song:verse-text verse-1)
                                                                              (song:verse-text verse-2))
                                                        #:notelist/rests (append (song:verse-notelist/rests verse-1)
                                                                                 (song:verse-notelist/rests verse-2))
                                                        #:unfinished (song:verse-unfinished verse-2))
                                       (cddr verse-list)))))
                       (else
                        (cons (car verse-list) (merge (cdr verse-list))))))))
      (song:debug "Final verses" (merge (append-map (lambda (lst) (append-map song:verse-block-verse-list lst))
                                                    (expand verse-block-list)))))))

(define (song:handle-music music)
  ;; Returns list of verses.
  ;; The main analysis function.
  (if song:*debug*
      (display-scheme-music music))
  (let ((score-list (song:debug "Final raw notes" (song:get-notes music)))
        (music-context-list (song:collect-lyrics-music music)))
    (for-each (lambda (music-context)
                (let ((context (song:music-context-context music-context)))
                  (song:insert-lyrics! (song:get-lyrics (song:music-context-music music-context) context)
                                       score-list context)
                  (song:debug "Final score list" score-list)))
              music-context-list)    
    (song:extract-verses score-list)))


;;; Output


(define song:festival-note-mapping '((0 "C") (1 "C#") (2 "D") (3 "D#") (4 "E") (5 "F") (6 "F#")
                                     (7 "G") (8 "G#") (9 "A") (10 "A#") (11 "B")))
(define (song:festival-pitch pitch)
  (let* ((semitones (ly:pitch-semitones pitch))
         (octave (inexact->exact (floor (/ semitones 12))))
         (tone (modulo semitones 12)))
    (format #f "~a~a" (cadr (assoc tone song:festival-note-mapping))
            (+ octave song:*base-octave* song:*base-octave-shift*))))

(define (song:write-header port tempo)
  (let ((beats (or (song:tempo->beats tempo) 100)))
    (format port "<?xml version=\"1.0\"?>
<!DOCTYPE SINGING PUBLIC \"-//SINGING//DTD SINGING mark up//EN\" \"Singing.v0_1.dtd\" []>
<SINGING BPM=\"~d\">
" beats)))

(define (song:write-footer port)
  (format port "</SINGING>~%"))

(define (song:write-lyrics port music)
  (for-each (lambda (verse)
              (let ((text (song:verse-text verse))
                    (note/rest-list (song:verse-notelist/rests verse)))
                (receive (rest-list note-listlist) (partition song:rest? note/rest-list)
                  (song:debug "Rest list" rest-list)
                  (song:debug "Note list" note-listlist)
                  (for-each (lambda (rest) (song:write-rest-element port rest)) rest-list)
                  (if (not (null? note-listlist))
                      (song:write-lyrics-element port text note-listlist)))))
            (song:handle-music music)))

(define (song:write-lyrics-element port text slur-list)
  (let ((fmt "~{~{~a~^+~}~^,~}")
        (transform (lambda (function)
                     (map (lambda (slur)
                            (let ((rests (filter song:rest? slur)))
                              (if (not (null? rests))
                                  (begin
                                    (song:warning (car rests) "Rests in a slur: ~a" slur)
                                    (set! slur (remove song:rest? slur)))))
                            (map function slur))
                          slur-list))))
    (format port "<DURATION BEATS=\"~@?\"><PITCH NOTE=\"~@?\">~a</PITCH></DURATION>~%"
            fmt (transform song:note-duration)
            fmt (transform (song:compose song:festival-pitch song:note-pitch))
            text)))

(define (song:write-rest-element port rest)
  (format port "<REST BEATS=\"~a\"></REST>~%" (song:rest-duration rest)))
