tests := lily1 lily2 lily3 lily4 lily5 lily6 lily6a lily7 lily7a lily8 lily9 lily-cs1

default: $(addsuffix .result,$(tests))
	ls -l *.result

music: $(addsuffix .wav,$(tests))

all: default music

%.xml: %.ly %-ok.xml ../festival.scm
	lilypond -o $(basename $@) $<

%.wav: %.xml ../singing-mode.scm
	text2wave -eval ../singing-mode.scm -eval '(voice_kal_diphone)' -mode singing $< -o $@

%.result: %.xml %-ok.xml
	-diff $+ >$@

clean:
	rm -f *.pdf *.ps *.wav *.result $(addsuffix .xml,$(tests))


.PHONY: default music all clean
.SECONDARY: $(addsuffix .xml,$(tests))
