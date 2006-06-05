#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily7a.xml" { \tempo 4 = 100 }
<<
  \relative \context Voice = "lahlah" {
    \set Staff.autoBeaming = ##f
    c4
    <<
      {
        \voiceTwo
        f8.[ g16]
        \oneVoice
      }
      \context Voice = alternative {
        \voiceOne
        \times 2/3 {
          \override NoteColumn #'force-hshift = #-3
          f8 f g
        }
      }
    >>
    a8( b) c
  }
  \new Lyrics \lyricsto "lahlah" {
    Ju -- ras -- sic Park
  }
  \new Lyrics \lyricsto "lahlah" {
    \set associatedVoice = alternative % applies to "ran"
    Ty --
    ran --
    no --
    \set associatedVoice = lahlah % applies to "rus"
    sau -- rus Rex
  } >>
