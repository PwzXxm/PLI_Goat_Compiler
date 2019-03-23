KidParser: KidParser.hs KidAST.hs
	ghc KidParser.hs

KidLexer: KidLexer.x
	alex KidLexer.x
	ghc -o KidLexer KidLexer.hs

clean:
	rm -f *.o *.hi
	rm -f KidParser KidLexer KidLexer.hs

