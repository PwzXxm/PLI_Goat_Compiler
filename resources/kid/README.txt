
With this README file you should also have found the following files:

Makefile:
    A rudimentary makefile for the COMP90045 project 2019.

KidLexer.x
    An alex specification for a subset, Kid, of our target language.
    Running alex with this specification as input will produce a
    lexer, called KidLexer, for Kid.
    It is quite possible that you will *not* need this file as a
    starting point for your Goat parser; it depends on which other 
    tools you plan to use. The main purpose of including the alex
    specification here is to be precise about lexical assumptions
    that carry over to Goat, such as the definition of a valid 
    identifier.

KidParser.hs
    A parser for Kid written using the Parsec parser combinator library.
    It depends on KidAST.hs. It does not depend on KidLexer.x or 
    KidLexer.hs, because Parsec also offers support for scanning. 
    Again, you can use this as a starting point for your Goat parser,
    or you may choose to do things differently. 

KidAST.hs
    Data structures to represent abstract syntax trees (ASTs) for Kid.  
    You may use this as a starting point for your design of similar
    data structures for Goat. Quite possibly you will not just need
    to extend the given definitions, you may have to alter them or
    redesign parts to suit the complexities of Goat.

hello.gt, io.gt, asg.gt:
    Three example Kid programs. The gt suffix is for "Goat" - since
    Kid is a proper subset of Goat, the examples are also valid Goat 
    programs.

To get started, study the files, compile and execute them.  On a
Unix machine you should be able to just type 

    make

and that should generate the executable KidParser for you. To build
a scanner, type

    make KidLexer

To run these on some Kid source programs, do

    KidLexer < io.gt
or
    KidParser io.gt

(the parser is written so that it understands command-line arguments.)

If your Unix system doesn't seem to recognise 'KidLexer' and 'KidParser',
that could be because your Unix PATH variable hasn't been set correctly.
For now, just try instead

    ./KidLexer < io.gt
and
    ./KidParser io.gt

