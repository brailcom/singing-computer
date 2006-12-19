Zpívající počítač (singing-computer)
====================================

Zpívající počítač je program umožňující přezpívat notový zápis (doplněný
textem) zapsaný v systému sazby not LilyPond.  Nástroj je určen především pro
zrakově postižené uživatele LilyPondu, kterým umožňuje kontrolu vzájemné pozice
textu a not.  Toho je dosaženo vložením nově definovaného lilypondového příkazu
do lilypondového zápisu, který způsobí vygenerování definičního souboru zpěvu
pro systém syntézy řeči Festival, s pomocí kterého se následně provede syntéza
zvukového souboru.

Pro práci se Zpívajícím počítačem se předpokládá základní znalost práce se
systémem LilyPond.  Tutoriál k LilyPondu je součástí instalace LilyPondu.

* Instalace

Nejprve je třeba nainstalovat následující software:

- LilyPond (http://www.lilypond.org) -- systém pro sazbu not.

- Festival (http://www.festvox.org/festival/index.html) -- systém syntézy řeči.

- Jazykovou podporu a hlasy Festivalu dle požadovaných jazyků.  Podpora pro
  angličtinu je dostupná přímo ze stránek Festivalu
  (http://www.festvox.org/festival/index.html), podpora pro češtinu je ke
  stažení na stránkách projektu Free(b)soft
  (http://www.freebsoft.org/festival-czech a
  http://www.freebsoft.org/festival-czech-diphone-database), odkazy na podporu
  dalších jazyků jsou na stránce http://festlang.berlios.de/languages.html.

- Iconv -- program pro konverzi znakových sad, který je součástí GNU libc.

Výše uvedený software je obvykle nejjednodušší nainstalovat z distribučních
balíků vašeho operačního systému.  Například v Debianu se jedná o následující
balíky: lilypond, festival, festvox-kallpc16k, festival-czech,
festvox-czech-ph.

Po instalaci výše uvedeného softwaru a rozbalení distribučního souboru
singing-computer* je ještě nutné provést následující kroky:

- Souborem singing-mode.scm z distribuce singing-computer nahradit soubor téhož
  jména z instalace Festivalu.

- Soubor festival.ly umístit mezi ostatní *.ly soubory instalace LilyPondu.

- Soubor festival.scm umístit mezi ostatní *.scm soubory instalace LilyPondu.

- Soubor `lilysong' umístěte do některé z cest v $PATH, např. /usr/local/bin/.

* Použití

Zpívající počítač definuje dva nové lilypondové příkazy \festival a
\festivalsyl, které fungují podobně jako příkaz \midi. Příkazy mají následující
podobu:

  \include "festival.ly"
  \festival #"SOUBOR.xml" { \tempo 4 = 100 }

Za příkazem musí následovat zápis hudby s textem.

Příkaz říká, že pro následující kus hudby se má vygenerovat soubor s názvem
SOUBOR.xml, ze kterého lze pomocí Festivalu následně vygenerovat odpovídající
zvukový soubor.  Definované tempo je 1/4 = 100.  Tempo se v příkazu \festival
zadává stejným způsobem jako v příkazu \midi.

Příkazy \festival a \festivalsyl mají stejný formát a funkci, liší se pouze
dělením slov na slabiky.  Příkaz \festival generuje ve výstupním XML souboru
celá slova, zatímco příkaz \festivalsyl v něm dělí slova na slabiky, tak jak je
vyznačeno ve vstupním lilypondovém souboru.  Každý z těchto příkazů se používá
pro jiné jazyky.  Pro češtinu se používá příkaz \festivalsyl, pro angličtinu
\festival.  Výsledný zvukový soubor volbou příkazu ovlivněn není, jeho správná
volba je však nutná pro správné provedení řečové syntézy.

V jednom vstupním souboru může být pro různé části vstupního souboru uvedeno
více příkazů \festival a \festivalsyl, s výstupem do různých souborů.

Příklady použití příkazů \festival a \festivalsyl jsou v souborech tests/*.ly.

Po zpracování vstupního souboru LilyPondem je příslušný XML soubor, nedojde-li
k chybě, vygenerován.  Tento soubor je nutno následně zpracovat Festivalem pro
vytvoření výsledného zvukového souboru určeného k přehrání.  Pro tuto operaci
je k dispozici skript `lilysong', který může být spuštěn některým
z následujících způsobů:

- lilysong [ -p PŘEHRÁVACÍ-PROGRAM ] SOUBOR.xml [ KÓD-JAZYKA-NEBO-HLAS [ ZRYCHLENÍ ] ]

  Vytvoří zvukový soubor odpovídající souboru SOUBOR.xml.  Jako parametr
  KÓD-JAZYKA-NEBO-HLAS volání je zadán kód jazyka (`en' nebo `cs') nebo přímo
  požadovaný festivalový hlas (např. `voice_kal_diphone' nebo
  `voice_czech_ph').  Nepovinným parametrem ZRYCHLENÍ je požadované zrychlení
  hlasu (viz odstavec „Problémy“ níže).  Je-li zadána volba `-p', je výsledný
  WAV soubor přehrán zadaným programem.

- lilysong SOUBOR.ly [ KÓD-JAZYKA-NEBO-HLAS ]

  Provede zpracováním lilypondem i Festivalem současně, případné zrychlení je
  odvozeno z obsahu zdrojového souboru.  Funguje pouze v případě, že se ze
  SOUBOR.ly generuje jediný zpěv.

Skript v obou případech vyrobí výsledný zvukový soubor SOUBOR.wav, který pak
lze přehrát libovolným přehrávačem zvukových souborů.

* Seznam varovných hlášení

Narazí-li Zpívající počítač na podezřelou situaci ve vstupním souboru, vypíše
některé z následujících varovných hlášení, označených jako `***Song
Warning***':

  Lyrics context not found

    Text by měl náležet do daného kontextu, ale tento kontext nebyl nalezen.
    Za chybovým hlášením je vypsáno jméno příslušného kontextu.

  Extra notes

    Byly nalezeny noty bez textu.
    Jedná se o noty, které zbyly v daném kontextu po zpracování veškerého textu.
    Za chybovým hlášením je vypsáno jméno kontextu a přebývající noty.

  Missing lyrics

    Byly nalezeny noty bez textu.
    Jedná se o noty, které zbyly v určitém úseku hudby po zpracování veškerého
    textu.
    Za chybovým hlášením je vypsáno jméno kontextu a přebývající noty.

  Extra lyrics

    Byl nalezen text bez odpovídajících not.
    Jedná se o text, který zbyl v daném kontextu po zpracování všech not.
    Za chybovým hlášením je vypsáno jméno kontextu a přebývající noty.

  Unfinished slur

    Byla detekována neukončená skupina vzájemně propojených not
    (např. obloučkem), některé noty na konci chybí.
    Za chybovým hlášením je vypsáno jméno kontextu a příslušná skupina not.

  Excessive skip

    V textu je vyznačeno přeskočení not, ale celková délka přeskočených not je
    kratší než délka přeskoku.
    Za chybovým hlášením je vypsáno jméno kontextu, délka přeskoku, přebytečná
    délka přeskoku a noty odpovídající přeskoku.

  Skip misalignment

    V textu je vyznačeno přeskočení not, ale celková délka přeskočených not
    neodpovídá délce přeskoku.
    Za chybovým hlášením je vypsáno jméno kontextu, délka přeskoku, přebytečná
    délka přeskoku a noty odpovídající přeskoku.

  Slur underrun

    Ve zdrojové specifikace se objevilo více ukončení než zahájení skupin not.

  Rests in a slur

    V obloučku se objevila pauza.
    Za chybovým hlášením je vypsán obsah daného obloučku.

Kromě těchto varovných hlášení se také mohou objevit chybová hlášení (označená
jako "error").  Ta jsou způsobena chybami programu a jejich výskyt by měl být
nahlášen vývojářům Zpívajícího počítače, viz odstavec Kontakt.

* Podpora v Emacsu

Pokud chcete kromě standardních lilypondových příkazů v Emacsu používat i
podporu pro syntézu zpěvu, zkopírujte soubor lilypond-song.el mezi ostatní *.el
soubory instalace LilyPondu a do svého ~/.emacs si přidejte řádek

  (require 'lilypond-song)

Podpora zpěvu v Emacsu doplní nové příkazy pro přezpívání textu syntetizovaného
lilypondovými příkazy \festival a \festivalsyl:

  M-x LilyPond-command-sing (C-c C-a)
  M-x LilyPond-command-sing-and-play
  M-x LilyPond-command-sing-last (C-c C-z)

Více informací naleznete v online dokumentaci těchto příkazů v Emacsu.

UPOZORNĚNÍ: Pokud je instalován Ecasound s podporou Emacsu, lilypond-song jej
automaticky použije.  Emacsová podpora Ecasoundu je však implicitně nastavena
na daemon mód, který obvykle nefunguje, je tedy potřeba ho odnastavit.  To se
provede nastavením emacsové proměnné ecasound-arguments, např. zrušením
zaškrtnutí volby "Allow remote connections" v customize.

UPOZORNĚNÍ: Při současném přehrávání zpěvu a MIDI se lze setkat s potíží
spočívající v tom, že timidity usekává počáteční pomlky v MIDI souboru.  To lze
potlačit aplikací patche ze souboru timidity.patch na zdrojový kód timidity a
následným překompilováním a reinstalací timidity.  Je též poté nutno přidat
volbu --preserve-silence do emacsové proměnné LilyPond-midi->wav-command.
  
* Problémy

Současné verze Festivalu trpí dosud neopravenou chybou, která u tónů vyšších
než 500 Hz způsobí pád Festivalu nebo vygenerování zpěvu nesprávné výšky.  Tuto
chybu lze obejít následujícími způsoby:

1. Aplikací záplaty `festival.patch' na Festival.  To je jediné spolehlivé
   řešení.  Ve zdrojovém adresáři Festivalu proveďte

     patch -p0 <.../singing-computer/festival.patch

   a Festival překompilujte.  Pozor, Festival nelze zkompilovat s g++ 4.0 a
   novějším, je potřeba použít starší verze g++.

2. Snížením oktávy generovaného výstupu nastavením proměnné
   song:*base-octave*.  To je nutné provést ještě před prvním použitím příkazů
   \festival nebo \festivalsyl následujícím způsobem:

     #(set! song:*base-octave* 3)

   Implicitní hodnotou proměnné song:*base-octave* je 5, výše uvedené nastavení
   způsobí snížení výsledného zpěvu o dvě oktávy.

3. Snížením výšky výstupu s jeho současným zpomalením a následným
   přesamplováním na správnou výšku.  Tento postup je mírně složitější.
   Nejprve se musí provést snížení výšky a zpomalení výstupu před prvním
   použitím příkazů \festival nebo \festivalsyl v lilypondovém zdrojovém
   souboru:

     #(set! song:*base-octave-shift* -2)

   Tím bude výstup snížen o dvě oktávy a zpomalen čtyřikrát.  Po vygenerování
   zvukového souboru lze tento přesamplovat příkazem

     sox SOUBOR.wav PŘESAMPLOVANÝ.wav 4

   Oproti prvnímu postupu se ve výsledku získá zpěv ve správné oktávě, výstup
   je však značně nekvalitní.

Nezávisle na této chybě platí, že Festival není schopen s momentálně dostupnými
difonovými databázemi produkovat kvalitní výsledek pro hlasy o velmi vysokých
výškách.

* Kontakt

Dotazy, oznámení o chybách a jiné podněty lze zasílat do mailing listu
singing-computer@lists.freebsoft.org.
