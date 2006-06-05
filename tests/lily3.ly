#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily3.xml" { \tempo 4 = 100 }
\relative c''
{
<<
  \context Voice = "lala" {
    \time 3/4
    f4 g8
    \melisma
    f e f
    \melismaEnd
    e2
  }
  \lyricsto "lala" \new Lyrics {
    la di __ daah
  }
>>
}
