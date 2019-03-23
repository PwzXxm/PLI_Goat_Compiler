SRC    = ./src
TARGET = ./build

Goat: $(SRC)/GoatLexer.x
	alex -o $(TARGET)/GoatLexer.hs $(SRC)/GoatLexer.x
	ghc -o $(TARGET)/Goat $(TARGET)/GoatLexer.hs


clean:
	rm -f $(TARGET)/*
