#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily6.xml" { \tempo 4 = 100 }
\relative { c c g' }
\addlyrics {
  twin -- \skip 4
  kle
}
