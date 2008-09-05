%.html: %.text
	pandoc -S -f markdown -t html -o $@ < $<

%.tex: %.text
	pandoc -S -s -f markdown -t latex -o $@ < $<

%.pdf: %.tex
	pdflatex $<
