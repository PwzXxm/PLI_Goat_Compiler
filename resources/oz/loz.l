%{
/*
**	Scanner for the Oz simulator.
*/

/*
#define	__USE_SVID
#define	__USE_POSIX
#define	__USE_XOPEN_EXTENDED
#define	_GNU_SOURCE
#define	__EXTENSIONS__

#undef	_STRING_H
*/

#include	<stdio.h>
#include	<string.h>
#include	"std.h"
#include	"oz.h"
#include	"poz.h"
#include	"missing.h"

#undef	ozyywrap
#define	YY_NO_UNPUT
#define	YY_NO_INPUT

extern	void	ozyyinit(void);
extern	int	ozyywrap(void);

const char	*ozyyfile = "no input file";
int		ozyylinenum = 0;

extern	int	ozyyget_lineno(void);
extern	FILE	*ozyyget_in(void);
extern	FILE	*ozyyget_out(void);
// extern	yy_size_t	ozyyget_leng(void);
extern	char	*ozyyget_text(void);
extern	void	ozyyset_lineno(int);
extern	void	ozyyset_in(FILE *);
extern	void	ozyyset_out(FILE *);
extern	int	ozyyget_debug(void);
extern	void	ozyyset_debug(int);
extern	int	ozyylex_destroy(void);

%}

alpha	[a-zA-Z_]
digit	[0-9]
alnum	[a-zA-Z_'0-9]
sp	[ \t]
nl	[\n\f]
nonl	[^\n\f]

%%

"push_stack_frame"	{ return INSTR_PUSH_STACK_FRAME; }
"pop_stack_frame"	{ return INSTR_POP_STACK_FRAME; }
"load"			{ return INSTR_LOAD; }
"store"			{ return INSTR_STORE; }
"load_address"		{ return INSTR_LOAD_ADDRESS; }
"load_indirect"		{ return INSTR_LOAD_INDIRECT; }
"store_indirect"	{ return INSTR_STORE_INDIRECT; }

"int_const"		{ return INSTR_INT_CONST; }
"real_const"		{ return INSTR_REAL_CONST; }
"string_const"		{ return INSTR_STRING_CONST; }

"add_int"		{ return INSTR_ADD_INT; }
"add_real"		{ return INSTR_ADD_REAL; }
"add_offset"		{ return INSTR_ADD_OFFSET; }
"sub_int"		{ return INSTR_SUB_INT; }
"sub_real"		{ return INSTR_SUB_REAL; }
"sub_offset"		{ return INSTR_SUB_OFFSET; }
"mul_int"		{ return INSTR_MUL_INT; }
"mul_real"		{ return INSTR_MUL_REAL; }
"div_int"		{ return INSTR_DIV_INT; }
"div_real"		{ return INSTR_DIV_REAL; }
"neg_int"		{ return INSTR_NEG_INT; }
"neg_real"		{ return INSTR_NEG_REAL; }

"cmp_eq_int"		{ return INSTR_CMP_EQ_INT; }
"cmp_ne_int"		{ return INSTR_CMP_NE_INT; }
"cmp_gt_int"		{ return INSTR_CMP_GT_INT; }
"cmp_ge_int"		{ return INSTR_CMP_GE_INT; }
"cmp_lt_int"		{ return INSTR_CMP_LT_INT; }
"cmp_le_int"		{ return INSTR_CMP_LE_INT; }
"cmp_eq_real"		{ return INSTR_CMP_EQ_REAL; }
"cmp_ne_real"		{ return INSTR_CMP_NE_REAL; }
"cmp_gt_real"		{ return INSTR_CMP_GT_REAL; }
"cmp_ge_real"		{ return INSTR_CMP_GE_REAL; }
"cmp_lt_real"		{ return INSTR_CMP_LT_REAL; }
"cmp_le_real"		{ return INSTR_CMP_LE_REAL; }
"cmp_eq_string"		{ return INSTR_CMP_EQ_STRING; }
"cmp_ne_string"		{ return INSTR_CMP_NE_STRING; }
"cmp_gt_string"		{ return INSTR_CMP_GT_STRING; }
"cmp_ge_string"		{ return INSTR_CMP_GE_STRING; }
"cmp_lt_string"		{ return INSTR_CMP_LT_STRING; }
"cmp_le_string"		{ return INSTR_CMP_LE_STRING; }

"and"			{ return INSTR_AND; }
"or"			{ return INSTR_OR; }
"not"			{ return INSTR_NOT; }

"branch_uncond"		{ return INSTR_BRANCH_UNCOND; }
"branch_on_true"	{ return INSTR_BRANCH_ON_TRUE; }
"branch_on_false"	{ return INSTR_BRANCH_ON_FALSE; }

"call"			{ return INSTR_CALL; }
"call_builtin"		{ return INSTR_CALL_BUILTIN; }
"return"		{ return INSTR_RETURN; }

"int_to_real"		{ return INSTR_INT_TO_REAL; }

"move"			{ return INSTR_MOVE; }

"debug_reg"		{ return INSTR_DEBUG_REG; }
"debug_slot"		{ return INSTR_DEBUG_SLOT; }
"debug_stack"		{ return INSTR_DEBUG_STACK; }

"halt"			{ return INSTR_HALT; }

"read_int"		{ return FUNC_READ_INT; }
"read_real"		{ return FUNC_READ_REAL; }
"read_bool"		{ return FUNC_READ_BOOL; }
"read_string"		{ return FUNC_READ_STRING; }

"print_int"		{ return FUNC_PRINT_INT; }
"print_real"		{ return FUNC_PRINT_REAL; }
"print_bool"		{ return FUNC_PRINT_BOOL; }
"print_string"		{ return FUNC_PRINT_STRING; }
"print_newline"		{ return FUNC_PRINT_NEWLINE; }

"string_concat"		{ return FUNC_STRING_CONCAT; }
"string_length"		{ return FUNC_STRING_LENGTH; }
"substring"		{ return FUNC_SUBSTRING; }

"sqrt"			{ return FUNC_SQRT; }
"trunc"			{ return FUNC_TRUNC; }
"round"			{ return FUNC_ROUND; }

","			{ return COMMA;	}
":"			{ return COLON;	}

"#"{nonl}*{nl}		{ ozyylinenum++; }

"-"?{digit}+		{ ozyylval.Uint = atoi(yytext);
			  return NAT_CONST;
			}

"-"?{digit}+"."{digit}+ { ozyylval.Ureal = atof(yytext);
			  return REAL_CONST;
			}

"\""([^\"]|\\.)*"\""	{ ozyylval.Ustr = strdup(yytext);
			  return STRING_CONST;
			}

"r"{digit}+		{ ozyylval.Uint = atoi(yytext+1);
			  return TOKEN_REG;
			}

{alpha}{alnum}*		{ ozyylval.Ustr = strdup(yytext);
			  return TOKEN_ID;
			}

{nl}			{ ozyylinenum++; }
{sp}			{}

{nonl} 			{ return GARBAGE; }

%%

void ozyyinit(void)
{
}

int ozyywrap(void)
{
	return 1;
}
