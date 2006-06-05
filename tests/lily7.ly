#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily7.xml" { \tempo 4 = 100 }
<<
  \relative \context Voice = "lahlah" {
    \set Staff.autoBeaming = ##f
    c4
    <<
      \context Voice = alternative {
        \voiceOne
        \times 2/3 {
          \override NoteColumn #'force-hshift = #-3
          f8 f g
        }
      }
      {
        \voiceTwo
        f8.[ g16]
        \oneVoice
      } >>
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
