SRC       = ./src
TARGET    = ./build

SRC_FILES = $(SRC)/Goat.hs $(SRC)/GoatAST.hs $(SRC)/GoatParser.hs $(SRC)/GoatToken.hs $(TARGET)/GoatLexer.hs

goat: $(SRC_FILES)
	ghc --make $(SRC)/Goat.hs -i$(SRC):$(TARGET) -hidir $(TARGET) -odir $(TARGET) -o ./goat 

$(TARGET)/GoatLexer.hs: $(SRC)/GoatLexer.x
	mkdir -p $(TARGET)
	alex -o $(TARGET)/GoatLexer.hs $(SRC)/GoatLexer.x

clean:
	rm -f $(TARGET)/* ./goat