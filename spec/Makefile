
.PHONY: all clean

all: r7rs.pdf

clean:
	rm -f *~

cleaner: clean
	rm -f *.pdf *.aux *.log *.idx *.toc

SOURCES=r7rs.tex basic.tex example.tex intro.tex prog.tex struct.tex \
	bib.tex expr.tex lex.tex syn.tex commands.tex first.tex notes.tex \
	repository.tex derive.tex index.tex procs.tex sem.tex

r7rs.pdf: $(SOURCES)
	pdflatex $<
