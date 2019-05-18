/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse ozyyparse
#define yylex   ozyylex
#define yyerror ozyyerror
#define yylval  ozyylval
#define yychar  ozyychar
#define yydebug ozyydebug
#define yynerrs ozyynerrs


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




/* Copy the first part of user declarations.  */
#line 1 "poz.y"

/*
**	Grammar for Oz programs.
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	"oz.h"
#include	"missing.h"

extern	char	*ozyytext;

extern	void	ozyyerror(const char *s);


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

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
/* Line 193 of yacc.c.  */
#line 290 "poz.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 303 "poz.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   275

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  82
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  7
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNRULES -- Number of states.  */
#define YYNSTATES  281

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   336

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,     9,    12,    15,    18,    23,
      28,    33,    38,    43,    48,    53,    58,    65,    72,    79,
      86,    93,   100,   107,   114,   121,   128,   133,   138,   145,
     152,   159,   166,   173,   180,   187,   194,   201,   208,   215,
     222,   229,   236,   243,   250,   257,   264,   271,   278,   283,
     286,   291,   296,   299,   302,   304,   309,   314,   317,   320,
     322,   324,   326,   328,   330,   332,   334,   336,   338,   340,
     342,   344,   346,   348,   350,   352,   354,   356
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      83,     0,    -1,    84,    -1,    84,    85,    -1,    -1,    76,
      74,    -1,     3,    77,    -1,     4,    77,    -1,     5,    87,
      73,    77,    -1,     6,    77,    73,    87,    -1,     7,    87,
      73,    77,    -1,     8,    87,    73,    87,    -1,     9,    87,
      73,    87,    -1,    10,    87,    73,    88,    -1,    11,    87,
      73,    79,    -1,    12,    87,    73,    80,    -1,    13,    87,
      73,    87,    73,    87,    -1,    14,    87,    73,    87,    73,
      87,    -1,    15,    87,    73,    87,    73,    87,    -1,    16,
      87,    73,    87,    73,    87,    -1,    17,    87,    73,    87,
      73,    87,    -1,    18,    87,    73,    87,    73,    87,    -1,
      19,    87,    73,    87,    73,    87,    -1,    20,    87,    73,
      87,    73,    87,    -1,    21,    87,    73,    87,    73,    87,
      -1,    22,    87,    73,    87,    73,    87,    -1,    23,    87,
      73,    87,    -1,    24,    87,    73,    87,    -1,    25,    87,
      73,    87,    73,    87,    -1,    26,    87,    73,    87,    73,
      87,    -1,    27,    87,    73,    87,    73,    87,    -1,    28,
      87,    73,    87,    73,    87,    -1,    29,    87,    73,    87,
      73,    87,    -1,    30,    87,    73,    87,    73,    87,    -1,
      31,    87,    73,    87,    73,    87,    -1,    32,    87,    73,
      87,    73,    87,    -1,    33,    87,    73,    87,    73,    87,
      -1,    34,    87,    73,    87,    73,    87,    -1,    35,    87,
      73,    87,    73,    87,    -1,    36,    87,    73,    87,    73,
      87,    -1,    37,    87,    73,    87,    73,    87,    -1,    38,
      87,    73,    87,    73,    87,    -1,    39,    87,    73,    87,
      73,    87,    -1,    40,    87,    73,    87,    73,    87,    -1,
      41,    87,    73,    87,    73,    87,    -1,    42,    87,    73,
      87,    73,    87,    -1,    43,    87,    73,    87,    73,    87,
      -1,    44,    87,    73,    87,    73,    87,    -1,    45,    87,
      73,    87,    -1,    46,    76,    -1,    47,    87,    73,    76,
      -1,    48,    87,    73,    76,    -1,    49,    76,    -1,    50,
      86,    -1,    51,    -1,    52,    87,    73,    87,    -1,    53,
      87,    73,    87,    -1,    54,    87,    -1,    55,    77,    -1,
      56,    -1,    57,    -1,    58,    -1,    59,    -1,    60,    -1,
      61,    -1,    62,    -1,    63,    -1,    64,    -1,    65,    -1,
      66,    -1,    67,    -1,    68,    -1,    69,    -1,    70,    -1,
      71,    -1,    72,    -1,    75,    -1,    77,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   133,   133,   136,   137,   140,   145,   151,   157,   164,
     171,   178,   185,   193,   200,   207,   215,   223,   231,   239,
     247,   255,   263,   271,   279,   287,   295,   302,   310,   318,
     326,   334,   342,   350,   359,   367,   375,   383,   391,   399,
     409,   417,   425,   433,   441,   449,   458,   466,   474,   482,
     488,   495,   503,   509,   515,   521,   529,   537,   543,   549,
     554,   561,   562,   563,   564,   565,   566,   567,   568,   569,
     570,   571,   572,   573,   574,   575,   578,   590
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INSTR_PUSH_STACK_FRAME",
  "INSTR_POP_STACK_FRAME", "INSTR_LOAD", "INSTR_STORE",
  "INSTR_LOAD_ADDRESS", "INSTR_LOAD_INDIRECT", "INSTR_STORE_INDIRECT",
  "INSTR_INT_CONST", "INSTR_REAL_CONST", "INSTR_STRING_CONST",
  "INSTR_ADD_INT", "INSTR_ADD_REAL", "INSTR_ADD_OFFSET", "INSTR_SUB_INT",
  "INSTR_SUB_REAL", "INSTR_SUB_OFFSET", "INSTR_MUL_INT", "INSTR_MUL_REAL",
  "INSTR_DIV_INT", "INSTR_DIV_REAL", "INSTR_NEG_INT", "INSTR_NEG_REAL",
  "INSTR_CMP_EQ_INT", "INSTR_CMP_NE_INT", "INSTR_CMP_GT_INT",
  "INSTR_CMP_GE_INT", "INSTR_CMP_LT_INT", "INSTR_CMP_LE_INT",
  "INSTR_CMP_EQ_REAL", "INSTR_CMP_NE_REAL", "INSTR_CMP_GT_REAL",
  "INSTR_CMP_GE_REAL", "INSTR_CMP_LT_REAL", "INSTR_CMP_LE_REAL",
  "INSTR_CMP_EQ_STRING", "INSTR_CMP_NE_STRING", "INSTR_CMP_GT_STRING",
  "INSTR_CMP_GE_STRING", "INSTR_CMP_LT_STRING", "INSTR_CMP_LE_STRING",
  "INSTR_AND", "INSTR_OR", "INSTR_NOT", "INSTR_BRANCH_UNCOND",
  "INSTR_BRANCH_ON_TRUE", "INSTR_BRANCH_ON_FALSE", "INSTR_CALL",
  "INSTR_CALL_BUILTIN", "INSTR_RETURN", "INSTR_INT_TO_REAL", "INSTR_MOVE",
  "INSTR_DEBUG_REG", "INSTR_DEBUG_SLOT", "INSTR_DEBUG_STACK", "INSTR_HALT",
  "FUNC_READ_INT", "FUNC_READ_REAL", "FUNC_READ_BOOL", "FUNC_READ_STRING",
  "FUNC_PRINT_INT", "FUNC_PRINT_REAL", "FUNC_PRINT_BOOL",
  "FUNC_PRINT_STRING", "FUNC_PRINT_NEWLINE", "FUNC_STRING_CONCAT",
  "FUNC_STRING_LENGTH", "FUNC_SUBSTRING", "FUNC_SQRT", "FUNC_TRUNC",
  "FUNC_ROUND", "COMMA", "COLON", "TOKEN_REG", "TOKEN_ID", "NAT_CONST",
  "INT_CONST", "REAL_CONST", "STRING_CONST", "GARBAGE", "$accept", "file",
  "instrs", "instr", "funcname", "reg", "int_const", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    82,    83,    84,    84,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    86,    86,    86,    86,    86,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    87,    88
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     0,     2,     2,     2,     4,     4,
       4,     4,     4,     4,     4,     4,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     4,     4,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     4,     2,
       4,     4,     2,     2,     1,     4,     4,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     0,     2,     1,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,    59,    60,     0,
       3,     6,     7,    76,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,     0,     0,    52,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    53,     0,     0,    57,    58,     5,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     8,     9,    10,    11,    12,
      77,    13,    14,    15,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    26,    27,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    50,    51,    55,
      56,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    60,   124,    64,   181
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -36
static const yytype_int16 yypact[] =
{
     -36,    39,    45,   -36,   -35,   -34,   -31,    26,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,    43,   -31,   -31,
      44,    46,   -36,   -31,   -31,   -31,    47,   -36,   -36,    48,
     -36,   -36,   -36,   -36,    54,    55,    56,    90,    91,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   -36,   130,   131,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   132,   133,   -36,   -36,   -36,
     134,   -31,   135,   -31,   -31,   166,   165,   167,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   169,   170,   -31,   -31,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   136,   137,   175,   176,   177,   178,
     179,   180,   181,   182,   -36,   -36,   183,   184,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,   202,   -36,   -36,   -36,   -36,
     -36,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -36,   -36,   -36,   -36,   -36,    -8,   -36
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint16 yytable[] =
{
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,     3,
     106,   107,    61,    62,    63,   125,   126,   127,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    65,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   105,
     108,    59,   129,   176,   128,   178,   179,   130,   131,   132,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,   201,   202,   203,
     204,   205,   206,   207,   208,   209,   210,   211,   212,   213,
     214,   215,   216,   133,   134,   219,   220,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,     0,     0,   221,
     222,   175,   177,   251,   252,   253,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   180,   182,   217,   218,   183,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250
};

static const yytype_int16 yycheck[] =
{
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,     0,
      48,    49,    77,    77,    75,    53,    54,    55,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    77,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    76,
      76,    76,    74,   131,    77,   133,   134,    73,    73,    73,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,    73,    73,   173,   174,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    -1,    -1,    73,
      73,    77,    77,   221,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   244,   245,   246,   247,
     248,   249,   250,    77,    79,    76,    76,    80,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    83,    84,     0,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    76,
      85,    77,    77,    75,    87,    77,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    76,    87,    87,    76,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    86,    87,    87,    87,    77,    74,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    77,    87,    77,    87,    87,
      77,    88,    79,    80,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    76,    76,    87,
      87,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 5:
#line 141 "poz.y"
    {
			record_label((yyvsp[(1) - (2)].Ustr));
		;}
    break;

  case 6:
#line 146 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_PUSH_STACK_FRAME;
			instr->int_const = (yyvsp[(2) - (2)].Uint);
		;}
    break;

  case 7:
#line 152 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_POP_STACK_FRAME;
			instr->int_const = (yyvsp[(2) - (2)].Uint);
		;}
    break;

  case 8:
#line 158 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->int_const = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 9:
#line 165 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_STORE;
			instr->int_const = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 10:
#line 172 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD_ADDRESS;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->int_const = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 11:
#line 179 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD_INDIRECT;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 12:
#line 186 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_STORE_INDIRECT;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 13:
#line 194 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_INT_CONST;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->int_const = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 14:
#line 201 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_REAL_CONST;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->real_const = (yyvsp[(4) - (4)].Ureal);
		;}
    break;

  case 15:
#line 208 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_STRING_CONST;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->string_const = (yyvsp[(4) - (4)].Ustr);
		;}
    break;

  case 16:
#line 216 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 17:
#line 224 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 18:
#line 232 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_OFFSET;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 19:
#line 240 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 20:
#line 248 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 21:
#line 256 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_OFFSET;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 22:
#line 264 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_MUL_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 23:
#line 272 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_MUL_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 24:
#line 280 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_DIV_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 25:
#line 288 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_DIV_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 26:
#line 296 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_NEG_INT;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 27:
#line 303 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_NEG_REAL;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 28:
#line 311 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 29:
#line 319 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 30:
#line 327 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 31:
#line 335 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 32:
#line 343 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 33:
#line 351 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_INT;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 34:
#line 360 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 35:
#line 368 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 36:
#line 376 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 37:
#line 384 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 38:
#line 392 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 39:
#line 400 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_REAL;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 40:
#line 410 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_STRING;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 41:
#line 418 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_STRING;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 42:
#line 426 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_STRING;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 43:
#line 434 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_STRING;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 44:
#line 442 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_STRING;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 45:
#line 450 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_STRING;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 46:
#line 459 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_AND;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 47:
#line 467 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_OR;
			instr->rd = (yyvsp[(2) - (6)].Uint);
			instr->rs1 = (yyvsp[(4) - (6)].Uint);
			instr->rs2 = (yyvsp[(6) - (6)].Uint);
		;}
    break;

  case 48:
#line 475 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_NOT;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 49:
#line 483 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_UNCOND;
			instr->string_const = (yyvsp[(2) - (2)].Ustr);
		;}
    break;

  case 50:
#line 489 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_ON_TRUE;
			instr->rs1 = (yyvsp[(2) - (4)].Uint);
			instr->string_const = (yyvsp[(4) - (4)].Ustr);
		;}
    break;

  case 51:
#line 496 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_ON_FALSE;
			instr->rs1 = (yyvsp[(2) - (4)].Uint);
			instr->string_const = (yyvsp[(4) - (4)].Ustr);
		;}
    break;

  case 52:
#line 504 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CALL;
			instr->string_const = (yyvsp[(2) - (2)].Ustr);
		;}
    break;

  case 53:
#line 510 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_CALL_BUILTIN;
			instr->func = (yyvsp[(2) - (2)].Ufunc);
		;}
    break;

  case 54:
#line 516 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_RETURN;
		;}
    break;

  case 55:
#line 522 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_INT_TO_REAL;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 56:
#line 530 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_MOVE;
			instr->rd = (yyvsp[(2) - (4)].Uint);
			instr->rs1 = (yyvsp[(4) - (4)].Uint);
		;}
    break;

  case 57:
#line 538 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_REG;
			instr->rs1 = (yyvsp[(2) - (2)].Uint);
		;}
    break;

  case 58:
#line 544 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_SLOT;
			instr->int_const = (yyvsp[(2) - (2)].Uint);
		;}
    break;

  case 59:
#line 550 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_STACK;
		;}
    break;

  case 60:
#line 555 "poz.y"
    {
			Instr *instr = record_instr();
			instr->opcode = OP_HALT;
		;}
    break;

  case 61:
#line 561 "poz.y"
    { (yyval.Ufunc) = FUNCOP_READ_INT; 	;}
    break;

  case 62:
#line 562 "poz.y"
    { (yyval.Ufunc) = FUNCOP_READ_REAL;	;}
    break;

  case 63:
#line 563 "poz.y"
    { (yyval.Ufunc) = FUNCOP_READ_BOOL;	;}
    break;

  case 64:
#line 564 "poz.y"
    { (yyval.Ufunc) = FUNCOP_READ_STRING;	;}
    break;

  case 65:
#line 565 "poz.y"
    { (yyval.Ufunc) = FUNCOP_PRINT_INT; 	;}
    break;

  case 66:
#line 566 "poz.y"
    { (yyval.Ufunc) = FUNCOP_PRINT_REAL;	;}
    break;

  case 67:
#line 567 "poz.y"
    { (yyval.Ufunc) = FUNCOP_PRINT_BOOL;	;}
    break;

  case 68:
#line 568 "poz.y"
    { (yyval.Ufunc) = FUNCOP_PRINT_STRING;	;}
    break;

  case 69:
#line 569 "poz.y"
    { (yyval.Ufunc) = FUNCOP_PRINT_NEWLINE;	;}
    break;

  case 70:
#line 570 "poz.y"
    { (yyval.Ufunc) = FUNCOP_STRING_CONCAT;	;}
    break;

  case 71:
#line 571 "poz.y"
    { (yyval.Ufunc) = FUNCOP_STRING_LENGTH;	;}
    break;

  case 72:
#line 572 "poz.y"
    { (yyval.Ufunc) = FUNCOP_SUBSTRING;	;}
    break;

  case 73:
#line 573 "poz.y"
    { (yyval.Ufunc) = FUNCOP_SQRT;		;}
    break;

  case 74:
#line 574 "poz.y"
    { (yyval.Ufunc) = FUNCOP_TRUNC;;}
    break;

  case 75:
#line 575 "poz.y"
    { (yyval.Ufunc) = FUNCOP_ROUND;;}
    break;

  case 76:
#line 579 "poz.y"
    {
			if ((yyvsp[(1) - (1)].Uint) < NUMBER_REGS) {
				(yyval.Uint) = (yyvsp[(1) - (1)].Uint);
			} else {
				printf("%s, %d: ", ozyyfile, ozyylinenum);
				printf("reference to r%d\n", (yyvsp[(1) - (1)].Uint));
				report_error_and_exit("");
			}
		;}
    break;

  case 77:
#line 591 "poz.y"
    {
			(yyval.Uint) = (yyvsp[(1) - (1)].Uint);
		;}
    break;


/* Line 1267 of yacc.c.  */
#line 2390 "poz.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 596 "poz.y"


void ozyyerror(const char *s)
{
	char		buf[80];

	if (ozyychar <= 0)
	{
		sprintf(buf, "premature EOF");
		ozyylinenum--;
	}
	else if (ozyytext[0] == '\n' || ozyytext[0] == '\f')
		sprintf(buf, "%s at end of line", s);
	else if (isprint((int) ozyytext[0]))
		sprintf(buf, "%s at symbol `%s'", s, ozyytext);
	else
		sprintf(buf, "%s at \\%o", s, ozyytext[0]);
	
	printf("%s, %d: %s\n", ozyyfile, ozyylinenum, buf);
}

