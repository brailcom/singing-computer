Zpívající počítač (singing-computer)
====================================

Zpívající počítač je program umožňující přezpívat notový zápis (doplněný
textem) zapsaný v systému sazby not LilyPond.  Nástroj je určen především pro
zrakově postižené uživatele LilyPondu, kterým umožňuje kontrolu vzájemné pozice
textu a not.  Toho je dosaženo vložením nově definovaného lilypondového příkazy
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

* Použití

Zpívající počítač definuje dva nové lilypondové příkaz \festival a
\festivalsyl, které fungují podobně jako příkaz \midi.  Příkazy mají
následující podobu:

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

Nezávisle na této chybě platí, že Festival není schopen produkovat kvalitní
výsledek pro hlasy o velmi vysokých výškách.

* Kontakt

Dotazy, oznámení o chybách a jiné podněty lze zasílat do mailing listu
singing-computer@lists.freebsoft.org.
