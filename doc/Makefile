# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)

.PHONY: build clean html

SRC = yi
EPS_FILES = yi_overview.eps

build: $(SRC).ps

$(SRC).ps: $(SRC).dvi
	dvips -t letter -o $@ $(SRC).dvi

$(SRC).pdf: $(SRC).dvi
	dvipdfm -o $@ $<

$(SRC).dvi: $(SRC).tex $(SRC).bib $(EPS_FILES)
	latex  $(SRC)
	bibtex $(SRC)
	latex  $(SRC)
	latex  $(SRC)

CLEANS= *.{bbl,blg,pdf,ps,dvi,aux,log} *~

clean: 
	rm -rf $(CLEANS)

all: doc

view: view-pdf

view-pdf: $(SRC).pdf
	open $<

show: $(SRC).ps
	gv -spartan $(SRC).ps

