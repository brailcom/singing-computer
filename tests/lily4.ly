#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily4.xml" { \tempo 4 = 100 }
\relative c'
{
<< \context Voice = melody {
     \time 3/4
     c2 e4 g2.
  }
  \new Lyrics \lyricmode {
    \set associatedVoice = #"melody"
    play2 the4 game2.
  } >>
}
