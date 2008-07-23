default: hask08.pdf

%.view: %.pdf  
	evince $<

%.print: %.ps
	lpr $<

%.pdf: %.tex %.bib
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

%.tex: %.text
	pandoc --tab-stop=2 -S -s -f markdown -t latex -o $@ < $<
