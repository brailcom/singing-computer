#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily10.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c2 e \breathe g }
\addlyrics { play the game }
}
