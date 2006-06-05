#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festival #"lily2.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c2 e4 g2. }
\addlyrics { play the game }
\addlyrics { speel het spel }
\addlyrics { joue le jeu }
}
