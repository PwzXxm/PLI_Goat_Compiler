# Make the Goat program 
# Authors:
#   Weizhi Xu  (752454)
#   Zijun Chen (813190)
#   Zhe Tang   (743398)

SRC       = .
TARGET    = ./build

SRC_FILES = $(SRC)/Goat.hs $(SRC)/GoatAST.hs $(SRC)/GoatParser.hs $(SRC)/GoatToken.hs $(SRC)/GoatFormatter.hs $(TARGET)/GoatLexer.hs

Goat: $(SRC_FILES)
	ghc --make $(SRC)/Goat.hs -i$(SRC):$(TARGET) -hidir $(TARGET) -odir $(TARGET) -o ./Goat -O2

$(TARGET)/GoatLexer.hs: $(SRC)/GoatLexer.x
	mkdir -p $(TARGET)
	alex -o $(TARGET)/GoatLexer.hs $(SRC)/GoatLexer.x

clean:
	rm -f $(TARGET)/* ./Goat
