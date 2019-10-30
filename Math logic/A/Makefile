all:
	ghc Main.hs

parser:
	alex Lexer.x -o Lexer.hs
	happy Parser.y -o Parser.hs

run:
	./Main

clean:
	rm -rf Main *.hi *.o Lexer.hs Parser.hs tt1.zip

zip:
	zip A.zip Makefile *.hs *.x *.y
