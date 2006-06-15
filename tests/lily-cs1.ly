#(define version-seen? #t)

#(set! %load-path (cons ".." %load-path))
\include "../festival.ly"
#(set! song:*debug* #f)

\festivalsyl #"lily-cs1.xml" { \tempo 4 = 100 }
{
\relative { c e g r }
\addlyrics { ov -- čá -- ci }
}
