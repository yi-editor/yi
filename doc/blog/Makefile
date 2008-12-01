ALL_TEXT := $(wildcard [0-9]*.text)

main.text: $(ALL_TEXT)
	cat $+ | grep -ve "]\:" > main.text

footer.text: $(ALL_TEXT)
	cat $+ | grep -e "]\:" > footer.text

all.text: header.text main.text footer.text $(ALL_TEXT)
	cat header.text main.text footer.text > $@



%.html: %.text
	pandoc -S -f markdown -t html -o $@ < $<

%.tex: %.text
	pandoc -S -s -f markdown -t latex -o $@ < $<

%.pdf: %.tex
	pdflatex $<

header.text:
	echo "%The Yi Blog" > $@