
.PHONY: all clean cleaner checkindex

all: r7rs.pdf

clean:
	rm -f *~

cleaner: clean
	rm -f *.pdf *.aux *.log *.idx *.toc

SOURCES=r7rs.tex basic.tex example.tex intro.tex prog.tex struct.tex \
	bib.tex expr.tex lex.tex syn.tex commands.tex first.tex notes.tex \
	repository.tex derive.tex index.tex procs.tex sem.tex stdmod-raw.tex \
	features.tex

# index.tex: r7rs.idx
# 	csi index.sch -e '(run)'

stdmod.tex: stdmod-raw.tex
	./genstdmod.pl < $< > $@

checkindex: stdmod-raw.tex index.tex
	./checkindex.sh

r7rs.pdf: $(SOURCES)
	pdflatex $<
	pdflatex $<

r5diff/%.tex: %.tex
	hg cat -r 1 $< > r5diff/old-$<
	latexdiff --type=UNDERLINE --subtype=SAFE r5diff/old-$< $< > $@
	rm -f r5diff/old-$<

r5diff/r7rs.pdf: $(SOURCES:%=r5diff/%)
	pdflatex -output-directory r5diff $<
	pdflatex -output-directory r5diff $<
