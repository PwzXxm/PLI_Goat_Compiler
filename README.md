# Goat Compiler

[![Build Status](https://travis-ci.com/PwzXxm/PLI_Goat_Compiler.svg?branch=master)](https://travis-ci.com/PwzXxm/PLI_Goat_Compiler)
![GitHub repo size](https://img.shields.io/github/repo-size/PwzXxm/PLI_Goat_Compiler)
![GitHub contributors](https://img.shields.io/github/contributors/PwzXxm/PLI_Goat_Compiler)
![GitHub](https://img.shields.io/github/license/PwzXxm/PLI_Goat_Compiler)


The Goat Compiler is written in Haskell utilizing lexical analyser ([Alex](https://www.haskell.org/alex/)), monadic parser ([Parsec](https://github.com/haskell/parsec)) and intermediate representation (Oz).

## Authors
- [Patrick Weizhi Xu](https://github.com/PwzXxm)
- [Zijun Chen (Zed)](https://github.com/CaviarChen)
- [Zhe Tang](https://github.com/dodojk)

## Goat Programming Language

You can access the specification of the Goat Programming Language [here](./resources/stage3.pdf).

An example program:

```
# sample.gt
proc p(val int x, val int y)
    int a[2];
begin
    a[x] := 42;
    a[y] := a[x];
    write a[y];
    write "\n";
end

proc main ()
begin
    call p(1,0);
end
```

## Usage

Build the compiler:
```
make
```

Display command line usage:
```
./Goat -h  
usage: Goat [-st | -sa | -sd | -p | -d | -r | -h] file
Options and arguments:
-st    : display secret tokens
-sa    : display secret Abstract Syntax Tree
-sd    : display secret Decorated Abstract Syntax Tree
-p     : pretty print the source file
-d     : debug build (no optimization)
-r     : release build (full optimization and remove comments)
-h     : display the help menu
file   : the file to be processed

default build (full optimization and keep comments)
```

**Note that the compiler only generate OZ Intermediate Representation, to run the program, you'll need the OZ VM.**  

To run a program, you can use the script 'single_test.sh':
```
./single_test.sh sample.gt
```

To run the compiler against all test cases:
```
python3 test.py
```



## Acknowledgement

This project is one of the assessments of COMP90045 Programming Language Implementation at the University of Melbourne. The lecturer Harald Søndergaard reserves all the rights.

The final mark of this project is 30 + 1 (bonus) out of 30.
