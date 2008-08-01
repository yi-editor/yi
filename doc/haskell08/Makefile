default: hask08.pdf

%.view: %.pdf  
	evince $<

%.print: %.ps
	lpr $<

%.ps: %.dvi
	dvips $< 

%.dvi: %.tex *.bib
	latex $<
	bibtex $*
	latex $<
	latex $<

%.pdf: %.tex *.bib
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

%.tex: %.text
	pandoc --tab-stop=2 -S -s -f markdown -t latex -o $@ < $<
