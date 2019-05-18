/*
** This module contains the infrastructure needed for executing Oz code.
*/

#ifndef	OZ_H
#define	OZ_H

#include "std.h"

typedef enum {
	OP_PUSH_STACK_FRAME,
	OP_POP_STACK_FRAME,
	OP_LOAD,
	OP_STORE,
	OP_LOAD_ADDRESS,
	OP_LOAD_INDIRECT,
	OP_STORE_INDIRECT,

	OP_INT_CONST,
	OP_REAL_CONST,
	OP_STRING_CONST,

	OP_ADD_INT,
	OP_ADD_REAL,
	OP_ADD_OFFSET,
	OP_SUB_INT,
	OP_SUB_REAL,
	OP_SUB_OFFSET,
	OP_MUL_INT,
	OP_MUL_REAL,
	OP_DIV_INT,
	OP_DIV_REAL,
	OP_NEG_INT,
	OP_NEG_REAL,

	OP_CMP_EQ_INT,
	OP_CMP_NE_INT,
	OP_CMP_GT_INT,
	OP_CMP_GE_INT,
	OP_CMP_LT_INT,
	OP_CMP_LE_INT,

	OP_CMP_EQ_REAL,
	OP_CMP_NE_REAL,
	OP_CMP_GT_REAL,
	OP_CMP_GE_REAL,
	OP_CMP_LT_REAL,
	OP_CMP_LE_REAL,

	OP_CMP_EQ_STRING,
	OP_CMP_NE_STRING,
	OP_CMP_GT_STRING,
	OP_CMP_GE_STRING,
	OP_CMP_LT_STRING,
	OP_CMP_LE_STRING,

	OP_AND,
	OP_OR,
	OP_NOT,

	OP_BRANCH_UNCOND,
	OP_BRANCH_ON_TRUE,
	OP_BRANCH_ON_FALSE,

	OP_CALL,
	OP_CALL_BUILTIN,
	OP_RETURN,

	OP_INT_TO_REAL,

	OP_MOVE,

	OP_DEBUG_REG,
	OP_DEBUG_SLOT,
	OP_DEBUG_STACK,

	OP_HALT
} Opcode;

typedef enum {
	FUNCOP_READ_INT,
	FUNCOP_READ_REAL,
	FUNCOP_READ_BOOL,
	FUNCOP_READ_STRING,

	FUNCOP_PRINT_INT,
	FUNCOP_PRINT_REAL,
	FUNCOP_PRINT_BOOL,
	FUNCOP_PRINT_STRING,
	FUNCOP_PRINT_NEWLINE,

	FUNCOP_STRING_CONCAT,
	FUNCOP_STRING_LENGTH,
	FUNCOP_SUBSTRING,
	FUNCOP_SQRT,
	FUNCOP_TRUNC,
	FUNCOP_ROUND,

	FUNCOP_LAST
} Function;

typedef struct {
	Opcode		opcode;
	Function	func;
	int		rd;
	int		rs1;
	int		rs2;
	int		int_const;
	float		real_const;
	bool		bool_const;
	char		*string_const;
} Instr;

typedef enum {
	TYPE_INT,
	TYPE_REAL,
	TYPE_STRING,
	TYPE_RETURN_ADDR,
	TYPE_FRAME_SIZE,
	TYPE_ADDRESS
} Type;

#define	NUMBER_REGS	1024

extern	Instr	*record_instr(void);
extern	void	record_label(char *name);

extern	void	report_error_and_exit(const char *msg);
extern	void	report_perror_and_exit(const char *msg);
extern	void	report_internal_error_and_exit(const char *msg);

extern	void	print_reg(int rnum);

#endif
