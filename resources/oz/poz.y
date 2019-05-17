%{
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
%}

%start		file

%union
{
	char		*Ustr;
	int		Uint;
	bool		Ubool;
	float		Ureal;
	Function	Ufunc;
}

%token	INSTR_PUSH_STACK_FRAME
%token	INSTR_POP_STACK_FRAME
%token	INSTR_LOAD
%token	INSTR_STORE
%token	INSTR_LOAD_ADDRESS
%token	INSTR_LOAD_INDIRECT
%token	INSTR_STORE_INDIRECT

%token	INSTR_INT_CONST
%token	INSTR_REAL_CONST
%token	INSTR_STRING_CONST

%token	INSTR_ADD_INT
%token	INSTR_ADD_REAL
%token	INSTR_ADD_OFFSET
%token	INSTR_SUB_INT
%token	INSTR_SUB_REAL
%token	INSTR_SUB_OFFSET
%token	INSTR_MUL_INT
%token	INSTR_MUL_REAL
%token	INSTR_DIV_INT
%token	INSTR_DIV_REAL
%token	INSTR_NEG_INT
%token	INSTR_NEG_REAL

%token	INSTR_CMP_EQ_INT
%token	INSTR_CMP_NE_INT
%token	INSTR_CMP_GT_INT
%token	INSTR_CMP_GE_INT
%token	INSTR_CMP_LT_INT
%token	INSTR_CMP_LE_INT

%token	INSTR_CMP_EQ_REAL
%token	INSTR_CMP_NE_REAL
%token	INSTR_CMP_GT_REAL
%token	INSTR_CMP_GE_REAL
%token	INSTR_CMP_LT_REAL
%token	INSTR_CMP_LE_REAL

%token	INSTR_CMP_EQ_STRING
%token	INSTR_CMP_NE_STRING
%token	INSTR_CMP_GT_STRING
%token	INSTR_CMP_GE_STRING
%token	INSTR_CMP_LT_STRING
%token	INSTR_CMP_LE_STRING

%token	INSTR_AND
%token	INSTR_OR
%token	INSTR_NOT

%token	INSTR_BRANCH_UNCOND
%token	INSTR_BRANCH_ON_TRUE
%token	INSTR_BRANCH_ON_FALSE
%token	INSTR_CALL
%token	INSTR_CALL_BUILTIN
%token	INSTR_RETURN

%token	INSTR_INT_TO_REAL

%token	INSTR_MOVE
%token	INSTR_DEBUG_REG
%token	INSTR_DEBUG_SLOT
%token	INSTR_DEBUG_STACK
%token	INSTR_HALT

%token	FUNC_READ_INT
%token	FUNC_READ_REAL
%token	FUNC_READ_BOOL
%token	FUNC_READ_STRING

%token	FUNC_PRINT_INT
%token	FUNC_PRINT_REAL
%token	FUNC_PRINT_BOOL
%token	FUNC_PRINT_STRING
%token	FUNC_PRINT_NEWLINE

%token	FUNC_STRING_CONCAT
%token	FUNC_STRING_LENGTH
%token	FUNC_SUBSTRING
%token	FUNC_SQRT
%token	FUNC_TRUNC
%token	FUNC_ROUND

%token	COMMA
%token	COLON

%token	<Uint>	TOKEN_REG
%token	<Ustr>	TOKEN_ID

%token	<Uint>	NAT_CONST

%token	<Uint>	INT_CONST
%token	<Ureal>	REAL_CONST
%token	<Ustr>	STRING_CONST

%token		GARBAGE

%type	<Ufunc>	funcname
%type	<Uint>	int_const
%type	<Uint>	reg

%%

/**********************************************************************/

file	:	instrs
	;

instrs	:	instrs instr
	|	/* empty */
	;

instr	:	TOKEN_ID COLON
      		{
			record_label($1);
		}

	|	INSTR_PUSH_STACK_FRAME NAT_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_PUSH_STACK_FRAME;
			instr->int_const = $2;
		}
	|	INSTR_POP_STACK_FRAME NAT_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_POP_STACK_FRAME;
			instr->int_const = $2;
		}
	|	INSTR_LOAD reg COMMA NAT_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD;
			instr->rd = $2;
			instr->int_const = $4;
		}
	|	INSTR_STORE NAT_CONST COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_STORE;
			instr->int_const = $2;
			instr->rs1 = $4;
		}
	|	INSTR_LOAD_ADDRESS reg COMMA NAT_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD_ADDRESS;
			instr->rd = $2;
			instr->int_const = $4;
		}
	|	INSTR_LOAD_INDIRECT reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD_INDIRECT;
			instr->rd = $2;
			instr->rs1 = $4;
		}
	|	INSTR_STORE_INDIRECT reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_STORE_INDIRECT;
			instr->rd = $2;
			instr->rs1 = $4;
		}

	|	INSTR_INT_CONST reg COMMA int_const
		{
			Instr *instr = record_instr();
			instr->opcode = OP_INT_CONST;
			instr->rd = $2;
			instr->int_const = $4;
		}
	|	INSTR_REAL_CONST reg COMMA REAL_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_REAL_CONST;
			instr->rd = $2;
			instr->real_const = $4;
		}
	|	INSTR_STRING_CONST reg COMMA STRING_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_STRING_CONST;
			instr->rd = $2;
			instr->string_const = $4;
		}

	|	INSTR_ADD_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_ADD_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_ADD_OFFSET reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_OFFSET;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_SUB_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_SUB_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_SUB_OFFSET reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_OFFSET;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_MUL_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_MUL_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_MUL_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_MUL_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_DIV_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_DIV_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_DIV_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_DIV_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_NEG_INT reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_NEG_INT;
			instr->rd = $2;
			instr->rs1 = $4;
		}
	|	INSTR_NEG_REAL reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_NEG_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
		}

	|	INSTR_CMP_EQ_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_NE_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_GT_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_GE_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_LT_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_LE_INT reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_INT;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}

	|	INSTR_CMP_EQ_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_NE_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_GT_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_GE_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_LT_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_LE_REAL reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}


	|	INSTR_CMP_EQ_STRING reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_STRING;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_NE_STRING reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_STRING;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_GT_STRING reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_STRING;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_GE_STRING reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_STRING;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_LT_STRING reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_STRING;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_CMP_LE_STRING reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_STRING;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}

	|	INSTR_AND reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_AND;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_OR reg COMMA reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_OR;
			instr->rd = $2;
			instr->rs1 = $4;
			instr->rs2 = $6;
		}
	|	INSTR_NOT reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_NOT;
			instr->rd = $2;
			instr->rs1 = $4;
		}

	|	INSTR_BRANCH_UNCOND TOKEN_ID
		{
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_UNCOND;
			instr->string_const = $2;
		}
	|	INSTR_BRANCH_ON_TRUE reg COMMA TOKEN_ID
		{
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_ON_TRUE;
			instr->rs1 = $2;
			instr->string_const = $4;
		}
	|	INSTR_BRANCH_ON_FALSE reg COMMA TOKEN_ID
		{
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_ON_FALSE;
			instr->rs1 = $2;
			instr->string_const = $4;
		}

	|	INSTR_CALL TOKEN_ID
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CALL;
			instr->string_const = $2;
		}
	|	INSTR_CALL_BUILTIN funcname
		{
			Instr *instr = record_instr();
			instr->opcode = OP_CALL_BUILTIN;
			instr->func = $2;
		}
	|	INSTR_RETURN
		{
			Instr *instr = record_instr();
			instr->opcode = OP_RETURN;
		}

	|	INSTR_INT_TO_REAL reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_INT_TO_REAL;
			instr->rd = $2;
			instr->rs1 = $4;
		}

	|	INSTR_MOVE reg COMMA reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_MOVE;
			instr->rd = $2;
			instr->rs1 = $4;
		}

	|	INSTR_DEBUG_REG reg
		{
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_REG;
			instr->rs1 = $2;
		}
	|	INSTR_DEBUG_SLOT NAT_CONST
		{
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_SLOT;
			instr->int_const = $2;
		}
	|	INSTR_DEBUG_STACK
		{
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_STACK;
		}
	|	INSTR_HALT
		{
			Instr *instr = record_instr();
			instr->opcode = OP_HALT;
		}
	;

funcname: 	FUNC_READ_INT		{ $$ = FUNCOP_READ_INT; 	}
	|	FUNC_READ_REAL		{ $$ = FUNCOP_READ_REAL;	}
	|	FUNC_READ_BOOL		{ $$ = FUNCOP_READ_BOOL;	}
	|	FUNC_READ_STRING 	{ $$ = FUNCOP_READ_STRING;	}
	| 	FUNC_PRINT_INT		{ $$ = FUNCOP_PRINT_INT; 	}
	|	FUNC_PRINT_REAL		{ $$ = FUNCOP_PRINT_REAL;	}
	|	FUNC_PRINT_BOOL		{ $$ = FUNCOP_PRINT_BOOL;	}
	|	FUNC_PRINT_STRING 	{ $$ = FUNCOP_PRINT_STRING;	}
	|	FUNC_PRINT_NEWLINE	{ $$ = FUNCOP_PRINT_NEWLINE;	}
	|	FUNC_STRING_CONCAT 	{ $$ = FUNCOP_STRING_CONCAT;	}
	|	FUNC_STRING_LENGTH 	{ $$ = FUNCOP_STRING_LENGTH;	}
	|	FUNC_SUBSTRING 		{ $$ = FUNCOP_SUBSTRING;	}
	|	FUNC_SQRT		{ $$ = FUNCOP_SQRT;		}
	|	FUNC_TRUNC		{ $$ = FUNCOP_TRUNC;}
	|	FUNC_ROUND		{ $$ = FUNCOP_ROUND;}
	;

reg	:	TOKEN_REG
     		{
			if ($1 < NUMBER_REGS) {
				$$ = $1;
			} else {
				printf("%s, %d: ", ozyyfile, ozyylinenum);
				printf("reference to r%d\n", $1);
				report_error_and_exit("");
			}
		}
	;

int_const:	NAT_CONST
	 	{
			$$ = $1;
		}
	;

%%

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
