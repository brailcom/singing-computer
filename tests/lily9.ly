#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\score{
\festival #"lily9.xml" { \tempo 4 = 100 }
<<
  \context Voice = melody \relative c' {
    c2 e4 r4 | g2 e | c1 |
    \context Voice = verse \repeat volta 2 {c4 d e f | g1 | }
    a2 b | c1}
  \lyricsto melody  \context Lyrics = mainlyrics \lyricmode {
    do mi sol mi do
    la si do }
  \lyricsto verse \context Lyrics = mainlyrics \lyricmode {
   do re mi fa sol }
  \lyricsto verse \context Lyrics = repeatlyrics \lyricmode {
   dodo rere mimi fafa solsol }
>>
}
