# Make the Goat program 
# Authors:
#   Weizhi Xu  (752454)
#   Zijun Chen (813190)
#   Zhe Tang   (743398)

SRC       = ./src
TARGET    = ./build

SRC_FILES = $(TARGET)/GoatLexer.hs $(SRC)/Goat.hs $(SRC)/GoatFormatter.hs $(SRC)/GoatToken.hs $(SRC)/GoatAST.hs $(SRC)/Makefile $(SRC)/GoatAnalyzer.hs $(SRC)/GoatOptimizer.hs $(SRC)/OzInstruction.hs $(SRC)/GoatCodeGenerator.hs $(SRC)/GoatParser.hs

Goat: $(SRC_FILES) oz
	ghc --make $(SRC)/Goat.hs -i$(SRC):$(TARGET) -hidir $(TARGET) -odir $(TARGET) -o ./Goat -O2

$(TARGET)/GoatLexer.hs: $(SRC)/GoatLexer.x
	mkdir -p $(TARGET)
	alex -o $(TARGET)/GoatLexer.hs $(SRC)/GoatLexer.x

oz:
	cd ./resources/oz && make

clean:
	rm -f $(TARGET)/* ./Goat ./oz
	cd ./resources/oz && make clean
