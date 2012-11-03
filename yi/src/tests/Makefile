
.PHONY: test

test: TestVim
	./TestVim

TestVim: TestVim.hs ../../dist/build/Yi/Lexer/common.hsinc
	ghc -odir build -hidir build -Wall -O2 -rtsopts --make -i../library -i../../dist/build -i../../dist/build/autogen -i../library/Yi/Lexer TestVim.hs

../../dist/build/Yi/Lexer/common.hsinc: ../library/Yi/Lexer/common.hsinc
	cp ../library/Yi/Lexer/common.hsinc ../../dist/build/Yi/Lexer

clean:
	rm TestVim
