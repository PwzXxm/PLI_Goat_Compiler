/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     INSTR_PUSH_STACK_FRAME = 258,
     INSTR_POP_STACK_FRAME = 259,
     INSTR_LOAD = 260,
     INSTR_STORE = 261,
     INSTR_LOAD_ADDRESS = 262,
     INSTR_LOAD_INDIRECT = 263,
     INSTR_STORE_INDIRECT = 264,
     INSTR_INT_CONST = 265,
     INSTR_REAL_CONST = 266,
     INSTR_STRING_CONST = 267,
     INSTR_ADD_INT = 268,
     INSTR_ADD_REAL = 269,
     INSTR_ADD_OFFSET = 270,
     INSTR_SUB_INT = 271,
     INSTR_SUB_REAL = 272,
     INSTR_SUB_OFFSET = 273,
     INSTR_MUL_INT = 274,
     INSTR_MUL_REAL = 275,
     INSTR_DIV_INT = 276,
     INSTR_DIV_REAL = 277,
     INSTR_NEG_INT = 278,
     INSTR_NEG_REAL = 279,
     INSTR_CMP_EQ_INT = 280,
     INSTR_CMP_NE_INT = 281,
     INSTR_CMP_GT_INT = 282,
     INSTR_CMP_GE_INT = 283,
     INSTR_CMP_LT_INT = 284,
     INSTR_CMP_LE_INT = 285,
     INSTR_CMP_EQ_REAL = 286,
     INSTR_CMP_NE_REAL = 287,
     INSTR_CMP_GT_REAL = 288,
     INSTR_CMP_GE_REAL = 289,
     INSTR_CMP_LT_REAL = 290,
     INSTR_CMP_LE_REAL = 291,
     INSTR_CMP_EQ_STRING = 292,
     INSTR_CMP_NE_STRING = 293,
     INSTR_CMP_GT_STRING = 294,
     INSTR_CMP_GE_STRING = 295,
     INSTR_CMP_LT_STRING = 296,
     INSTR_CMP_LE_STRING = 297,
     INSTR_AND = 298,
     INSTR_OR = 299,
     INSTR_NOT = 300,
     INSTR_BRANCH_UNCOND = 301,
     INSTR_BRANCH_ON_TRUE = 302,
     INSTR_BRANCH_ON_FALSE = 303,
     INSTR_CALL = 304,
     INSTR_CALL_BUILTIN = 305,
     INSTR_RETURN = 306,
     INSTR_INT_TO_REAL = 307,
     INSTR_MOVE = 308,
     INSTR_DEBUG_REG = 309,
     INSTR_DEBUG_SLOT = 310,
     INSTR_DEBUG_STACK = 311,
     INSTR_HALT = 312,
     FUNC_READ_INT = 313,
     FUNC_READ_REAL = 314,
     FUNC_READ_BOOL = 315,
     FUNC_READ_STRING = 316,
     FUNC_PRINT_INT = 317,
     FUNC_PRINT_REAL = 318,
     FUNC_PRINT_BOOL = 319,
     FUNC_PRINT_STRING = 320,
     FUNC_PRINT_NEWLINE = 321,
     FUNC_STRING_CONCAT = 322,
     FUNC_STRING_LENGTH = 323,
     FUNC_SUBSTRING = 324,
     FUNC_SQRT = 325,
     FUNC_TRUNC = 326,
     FUNC_ROUND = 327,
     COMMA = 328,
     COLON = 329,
     TOKEN_REG = 330,
     TOKEN_ID = 331,
     NAT_CONST = 332,
     INT_CONST = 333,
     REAL_CONST = 334,
     STRING_CONST = 335,
     GARBAGE = 336
   };
#endif
/* Tokens.  */
#define INSTR_PUSH_STACK_FRAME 258
#define INSTR_POP_STACK_FRAME 259
#define INSTR_LOAD 260
#define INSTR_STORE 261
#define INSTR_LOAD_ADDRESS 262
#define INSTR_LOAD_INDIRECT 263
#define INSTR_STORE_INDIRECT 264
#define INSTR_INT_CONST 265
#define INSTR_REAL_CONST 266
#define INSTR_STRING_CONST 267
#define INSTR_ADD_INT 268
#define INSTR_ADD_REAL 269
#define INSTR_ADD_OFFSET 270
#define INSTR_SUB_INT 271
#define INSTR_SUB_REAL 272
#define INSTR_SUB_OFFSET 273
#define INSTR_MUL_INT 274
#define INSTR_MUL_REAL 275
#define INSTR_DIV_INT 276
#define INSTR_DIV_REAL 277
#define INSTR_NEG_INT 278
#define INSTR_NEG_REAL 279
#define INSTR_CMP_EQ_INT 280
#define INSTR_CMP_NE_INT 281
#define INSTR_CMP_GT_INT 282
#define INSTR_CMP_GE_INT 283
#define INSTR_CMP_LT_INT 284
#define INSTR_CMP_LE_INT 285
#define INSTR_CMP_EQ_REAL 286
#define INSTR_CMP_NE_REAL 287
#define INSTR_CMP_GT_REAL 288
#define INSTR_CMP_GE_REAL 289
#define INSTR_CMP_LT_REAL 290
#define INSTR_CMP_LE_REAL 291
#define INSTR_CMP_EQ_STRING 292
#define INSTR_CMP_NE_STRING 293
#define INSTR_CMP_GT_STRING 294
#define INSTR_CMP_GE_STRING 295
#define INSTR_CMP_LT_STRING 296
#define INSTR_CMP_LE_STRING 297
#define INSTR_AND 298
#define INSTR_OR 299
#define INSTR_NOT 300
#define INSTR_BRANCH_UNCOND 301
#define INSTR_BRANCH_ON_TRUE 302
#define INSTR_BRANCH_ON_FALSE 303
#define INSTR_CALL 304
#define INSTR_CALL_BUILTIN 305
#define INSTR_RETURN 306
#define INSTR_INT_TO_REAL 307
#define INSTR_MOVE 308
#define INSTR_DEBUG_REG 309
#define INSTR_DEBUG_SLOT 310
#define INSTR_DEBUG_STACK 311
#define INSTR_HALT 312
#define FUNC_READ_INT 313
#define FUNC_READ_REAL 314
#define FUNC_READ_BOOL 315
#define FUNC_READ_STRING 316
#define FUNC_PRINT_INT 317
#define FUNC_PRINT_REAL 318
#define FUNC_PRINT_BOOL 319
#define FUNC_PRINT_STRING 320
#define FUNC_PRINT_NEWLINE 321
#define FUNC_STRING_CONCAT 322
#define FUNC_STRING_LENGTH 323
#define FUNC_SUBSTRING 324
#define FUNC_SQRT 325
#define FUNC_TRUNC 326
#define FUNC_ROUND 327
#define COMMA 328
#define COLON 329
#define TOKEN_REG 330
#define TOKEN_ID 331
#define NAT_CONST 332
#define INT_CONST 333
#define REAL_CONST 334
#define STRING_CONST 335
#define GARBAGE 336




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 20 "poz.y"
{
	char		*Ustr;
	int		Uint;
	bool		Ubool;
	float		Ureal;
	Function	Ufunc;
}
/* Line 1529 of yacc.c.  */
#line 219 "poz.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE ozyylval;

