
.PHONY: test

test: TestVim
	./TestVim

digest: TestVim
	@./TestVim | perl -ne 'print if /FAILED /' | cut -f2 -d' ' | cut -f1 -d'/' | uniq -c

TestVim: TestVim.hs ../../dist/build/Yi/Lexer/common.hsinc
	ghc -odir build -hidir build -Wall -O2 -rtsopts --make -i../library -i../../dist/build -i../../dist/build/autogen -i../library/Yi/Lexer TestVim.hs

TestExParser: TestExParser.hs ../../dist/build/Yi/Lexer/common.hsinc
	ghc -odir build -hidir build -Wall -O2 -rtsopts --make -i../library -i../../dist/build -i../../dist/build/autogen -i../library/Yi/Lexer TestExParser.hs

../../dist/build/Yi/Lexer/common.hsinc: ../library/Yi/Lexer/common.hsinc
	cp ../library/Yi/Lexer/common.hsinc ../../dist/build/Yi/Lexer

clean:
	rm TestVim
