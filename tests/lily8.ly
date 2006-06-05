#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\score{
\festival #"lily8.xml" { \tempo 4 = 100 }
<<
  \context Voice = "melody" {
    \relative c' {
      c4
      <<
        { \voiceOne c8 e }
        \context Voice = splitpart { \voiceTwo c4 }
      >>
      \oneVoice c4 c | c
    }
  }
  \new Lyrics \lyricsto "melody" { we shall not o- ver- come }
  \new Lyrics \lyricsto "splitpart" { will }
>> }
