/*
** Infrastructure for simulating Oz programs.
**
** Original emulator by Zoltan Somogyi.
** Changes and extensions by Harald Sondergaard and by Matt Giuca.
*/

#include    <stdio.h>
#include    <stdlib.h>
#include    <assert.h>
#include    <string.h>
#include    <math.h>
#include    "std.h"
#include    "oz.h"
#include    "missing.h"

extern  char    *strdup(const char *);

/***********************************************************************/

/*
** Simulator data structures.
*/

typedef struct {
    bool        store_valid;
    Type        store_type;
    int         int_val;
    float       real_val;
    char        *string_val;
} Storage;

/* The maximum number of instructions in the program */
#define MAX_NUM_INSTRS      50000

/* The maximum number of labels in the program */
#define MAX_NUM_LABELS      5000

#define STACK_SIZE          65536

static  Storage regs[NUMBER_REGS];
static  Storage stack[STACK_SIZE];

static  int top_slot = -1;
static  int cur_frame_size = -1;

#define stack_slot(n)       stack[top_slot - (n)]

static Instr    instrs[MAX_NUM_INSTRS];
static int      next_instr = 0;

typedef struct {
    char    *label_name;
    int     label_pc;
} Label;

static  Label   labels[MAX_NUM_LABELS];
static  int     next_label = 0;

static const char *func_names[] = {
    "read_int",
    "read_real",
    "read_bool",
    "read_string",

    "print_int",
    "print_real",
    "print_bool",
    "print_string",
    "print_newline",

    "string_concat",
    "string_length",
    "substring",

    "sqrt",
    "trunc",
    "round",
};

/***********************************************************************/

/*
** Housekeeping for the simulator.
*/

static  bool    print_instrs = FALSE;
static  bool    quiet = FALSE;
static  bool    statistics = FALSE;

static  bool    found_error = FALSE;
static  bool    halted = FALSE;
static  int     pc = 0;
static  int     next_pc = 0;
static  int     cur_instr = 0;

/***********************************************************************/

static  void    usage(void);
static  void    execute_instr_at_pc(void);
static  void    *checked_malloc(int num_bytes);
static  char    *make_string_const(char *);

static  int     oztrunc(float f);
static  int     ozround(float f);

static  bool    check_instr(int which);
static  int     lookup_label(char *name);

/***********************************************************************/

static  void    print_dynamic_context(void);

static  void    init_all_regs(void);
static  void    init_stack(void);
static  bool    stack_check_overflow(void);

static  void    set_reg_any(int reg, int src);
static  void    set_reg_int(int reg, int value);
static  void    set_reg_real(int reg, float value);
static  void    set_reg_string(int reg, char *value);
static  void    set_reg_addr(int reg, int offset);

static  void    check_reg(int reg);
static  void    check_slot(int slot, bool check_valid, bool check_frame);
static  void    check_offset(int offset, bool check_valid, bool check_frame);

static  int     int_reg(int reg);
static  float   real_reg(int reg);
static  char    *string_reg(int reg);
static  int     addr_reg(int reg);

static  int     int_slot(int slot);
static  float   real_slot(int slot);
static  char    *string_slot(int slot);

/***********************************************************************/

int
main(int argc, char **argv)
{
    const char  *filename;
    int         i;

    /* Process command line */

    if (argc < 2) {
        usage();
        exit(EXIT_FAILURE);
    }

    for (i = 1; i < argc - 1; i++) 
        if (streq(argv[i],"-i"))
            print_instrs = TRUE;
        else if (streq(argv[i],"-q"))
            quiet = TRUE;
        else if (streq(argv[i],"-s"))
            statistics = TRUE;
        else
            usage();
    
    filename = argv[argc-1];

    ozyyin = fopen(filename, "r");
    ozyyfile = filename;
    ozyylinenum = 1;
    if (ozyyin == NULL) {
        perror(filename);
        exit(EXIT_FAILURE);
    }

    if (ozyyparse() != 0) {
        /* the error message will already have been printed */
        exit(EXIT_FAILURE);
    }

    init_all_regs();
    init_stack();

    pc = 0;
    cur_instr = 0;
    while (! halted && ! found_error) {
        if (! check_instr(pc)) 
            report_error_and_exit("execution fell out of the program");
        next_pc = pc + 1;
        execute_instr_at_pc();
        cur_instr++;
        pc = next_pc;
    }

    if (found_error) 
        exit(EXIT_FAILURE);

    if (statistics) {
        printf("Number of instructions in program code: %d.\n", next_instr);
        printf("Number of instructions executed: %d.\n", cur_instr);
    }

    return 0;
}

static void
usage(void) {

    printf("usage: oz [-i] [-q] [-s] filename\n");
    printf("            -i Print instructions\n");
    printf("            -q Quiet\n");
    printf("            -s Statistics\n");
    exit(EXIT_FAILURE);
}

static void
execute_instr_at_pc(void) {

    Instr       *instr;
    int         i1, i2;
    float       r1, r2;
    char        *s1, *s2;
    int         l;
    char        *s;
    const char  *builtin_func;
    int         slot;
    int         offset;
    char        buf[1024];

    if (print_instrs) 
        printf("instr %d, pc %d: ", cur_instr, pc);

    instr = &instrs[pc];
    switch (instr->opcode) {

    case OP_PUSH_STACK_FRAME:
        if (print_instrs) 
            printf("push_stack_frame %d\n", instr->int_const);
        top_slot++;
        if (stack_check_overflow())
            break;
        stack[top_slot].store_valid = TRUE;
        stack[top_slot].store_type = TYPE_FRAME_SIZE;
        stack[top_slot].int_val = cur_frame_size;
        cur_frame_size = instr->int_const;
        top_slot += instr->int_const;
        if (stack_check_overflow())
            break;
        break;

    case OP_POP_STACK_FRAME:
        if (print_instrs) 
            printf("pop_stack_frame %d\n", instr->int_const);

        if (instr->int_const != cur_frame_size) {
            fprintf(stderr, "pop_stack_frame %d doesn't match "
                "previous push_stack_frame %d\n",
                instr->int_const, cur_frame_size);
            found_error = TRUE;
            break;
        }

        for (slot = 0; slot < instr->int_const; slot++) 
            stack_slot(slot).store_valid = FALSE;

        top_slot -= instr->int_const;

        cur_frame_size = stack[top_slot].int_val;
        stack[top_slot].store_valid = FALSE;
        top_slot--;
        break;

    case OP_LOAD:
        if (print_instrs) 
            printf("load r%d, %d\n", instr->rd, instr->int_const);

        check_slot(instr->int_const, TRUE, TRUE);
        regs[instr->rd] = stack_slot(instr->int_const);
        break;

    case OP_STORE:
        if (print_instrs) 
            printf("store %d, r%d\n", instr->int_const, instr->rs1);

        check_reg(instr->rs1);
        check_slot(instr->int_const, FALSE, TRUE);
        stack_slot(instr->int_const) = regs[instr->rs1];
        break;

    case OP_LOAD_ADDRESS:
        if (print_instrs) 
            printf("load_address r%d, %d\n", instr->rd, instr->int_const);

        check_slot(instr->int_const, FALSE, TRUE);
        set_reg_addr(instr->rd, top_slot - instr->int_const);
        break;

    case OP_LOAD_INDIRECT:
         if (print_instrs) 
            printf("load_indirect r%d, r%d\n", instr->rd, instr->rs1);

        offset = addr_reg(instr->rs1);
        check_offset(offset, TRUE, TRUE);       /* Must be valid */
        regs[instr->rd] = stack[offset];
        break;

    case OP_STORE_INDIRECT:
        if (print_instrs) 
            printf("store_indirect r%d, r%d\n", instr->rd, instr->rs1);

        check_reg(instr->rs1);
        offset = addr_reg(instr->rd);
        check_offset(offset, FALSE, TRUE);      /* Need not be valid */
        stack[offset] = regs[instr->rs1];
        break;

    case OP_INT_CONST:
        if (print_instrs) 
            printf("int_const r%d, %d\n", instr->rd, instr->int_const);

        set_reg_int(instr->rd, instr->int_const);
        break;

    case OP_REAL_CONST:
        if (print_instrs) 
            printf("real_const r%d, %f\n", instr->rd, instr->real_const);

        set_reg_real(instr->rd, instr->real_const);
        break;

    case OP_STRING_CONST:
        if (print_instrs) 
            printf("string_const r%d, %s\n", instr->rd, instr->string_const);

        set_reg_string(instr->rd, make_string_const(instr->string_const));
        break;

    case OP_ADD_INT:
        if (print_instrs) 
            printf("add_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 + i2);
        break;

    case OP_ADD_REAL:
        if (print_instrs) 
            printf("add_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_real(instr->rd, r1 + r2);
        break;

    case OP_ADD_OFFSET:
        if (print_instrs) 
            printf("add_offset r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        offset = addr_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        offset += i2;
        check_offset(offset, FALSE, TRUE);
        set_reg_addr(instr->rd, offset);
        break;

    case OP_SUB_INT:
        if (print_instrs) 
            printf("sub_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 - i2);
        break;

    case OP_SUB_REAL:
        if (print_instrs) 
            printf("sub_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_real(instr->rd, r1 - r2);
        break;

    case OP_SUB_OFFSET:
        if (print_instrs) 
            printf("sub_offset r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        offset = addr_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        offset -= i2;
        check_offset(offset, FALSE, TRUE);
        set_reg_addr(instr->rd, offset);
        break;

    case OP_MUL_INT:
        if (print_instrs) 
            printf("mul_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 * i2);
        break;

    case OP_MUL_REAL:
        if (print_instrs) 
            printf("mul_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_real(instr->rd, r1 * r2);
        break;

    case OP_DIV_INT:
        if (print_instrs) 
            printf("div_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 / i2);
        break;

    case OP_DIV_REAL:
        if (print_instrs) 
            printf("div_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_real(instr->rd, r1 / r2);
        break;

    case OP_NEG_INT:
        if (print_instrs) 
            printf("neg_int r%d, r%d\n",
                instr->rd, instr->rs1);

        i1 = int_reg(instr->rs1);
        set_reg_int(instr->rd, -i1);
        break;

    case OP_NEG_REAL:
        if (print_instrs) 
            printf("neg_real r%d, r%d\n",
                instr->rd, instr->rs1);

        r1 = real_reg(instr->rs1);
        set_reg_real(instr->rd, -r1);
        break;

    case OP_CMP_EQ_INT:
        if (print_instrs) 
            printf("cmp_eq_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 == i2);
        break;

    case OP_CMP_NE_INT:
        if (print_instrs) 
            printf("cmp_ne_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 != i2);
        break;

    case OP_CMP_GT_INT:
        if (print_instrs) 
            printf("cmp_gt_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 > i2);
        break;

    case OP_CMP_GE_INT:
        if (print_instrs) 
            printf("cmp_ge_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 >= i2);
        break;

    case OP_CMP_LT_INT:
        if (print_instrs) 
            printf("cmp_lt_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 < i2);
        break;

    case OP_CMP_LE_INT:
        if (print_instrs) 
            printf("cmp_le_int r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 <= i2);
        break;

    case OP_CMP_EQ_REAL:
        if (print_instrs) 
            printf("cmp_eq_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_int(instr->rd, r1 == r2);
        break;

    case OP_CMP_NE_REAL:
        if (print_instrs) 
            printf("cmp_ne_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_int(instr->rd, r1 != r2);
        break;

    case OP_CMP_GT_REAL:
        if (print_instrs) 
            printf("cmp_gt_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_int(instr->rd, r1 > r2);
        break;

    case OP_CMP_GE_REAL:
        if (print_instrs) 
            printf("cmp_ge_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_int(instr->rd, r1 >= r2);
        break;

    case OP_CMP_LT_REAL:
        if (print_instrs) 
            printf("cmp_lt_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_int(instr->rd, r1 < r2);
        break;

    case OP_CMP_LE_REAL:
        if (print_instrs) 
            printf("cmp_le_real r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        r1 = real_reg(instr->rs1);
        r2 = real_reg(instr->rs2);
        set_reg_int(instr->rd, r1 <= r2);
        break;

    case OP_CMP_EQ_STRING:
        if (print_instrs) 
            printf("cmp_eq_string r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        s1 = string_reg(instr->rs1);
        s2 = string_reg(instr->rs2);
        set_reg_int(instr->rd, strcmp(s1, s2) == 0);
        break;

    case OP_CMP_NE_STRING:
        if (print_instrs) 
            printf("cmp_ne_string r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        s1 = string_reg(instr->rs1);
        s2 = string_reg(instr->rs2);
        set_reg_int(instr->rd, strcmp(s1, s2) != 0);
        break;

    case OP_CMP_GT_STRING:
        if (print_instrs) 
            printf("cmp_gt_string r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        s1 = string_reg(instr->rs1);
        s2 = string_reg(instr->rs2);
        set_reg_int(instr->rd, strcmp(s1, s2) > 0);
        break;

    case OP_CMP_GE_STRING:
        if (print_instrs) 
            printf("cmp_ge_string r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        s1 = string_reg(instr->rs1);
        s2 = string_reg(instr->rs2);
        set_reg_int(instr->rd, strcmp(s1, s2) >= 0);
        break;

    case OP_CMP_LT_STRING:
        if (print_instrs) 
            printf("cmp_lt_string r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        s1 = string_reg(instr->rs1);
        s2 = string_reg(instr->rs2);
        set_reg_int(instr->rd, strcmp(s1, s2) < 0);
        break;

    case OP_CMP_LE_STRING:
        if (print_instrs) 
            printf("cmp_le_string r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);

        s1 = string_reg(instr->rs1);
        s2 = string_reg(instr->rs2);
        set_reg_int(instr->rd, strcmp(s1, s2) <= 0);
        break;

    case OP_AND:
        if (print_instrs) 
            printf("and r%d, r%d, r%d\n", instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 && i2);
        break;

    case OP_OR:
        if (print_instrs) 
            printf("or r%d, r%d, r%d\n", instr->rd, instr->rs1, instr->rs2);

        i1 = int_reg(instr->rs1);
        i2 = int_reg(instr->rs2);
        set_reg_int(instr->rd, i1 || i2);
        break;

    case OP_NOT:
        if (print_instrs) 
            printf("not r%d, r%d\n", instr->rd, instr->rs1);

        i1 = int_reg(instr->rs1);
        set_reg_int(instr->rd, !i1);
        break;

    case OP_BRANCH_UNCOND:
        if (print_instrs) 
            printf("branch_uncond %s\n", instr->string_const);

        next_pc = lookup_label(instr->string_const);
        break;

    case OP_BRANCH_ON_TRUE:
        if (print_instrs) 
            printf("branch_on_true r%d, %s\n",
                instr->rs1, instr->string_const);

        i1 = int_reg(instr->rs1);
        if (i1) 
            next_pc = lookup_label(instr->string_const);
        break;

    case OP_BRANCH_ON_FALSE:
        if (print_instrs) 
            printf("branch_on_false r%d, %s\n",
                instr->rs1, instr->string_const);

        i1 = int_reg(instr->rs1);
        if (! i1) 
            next_pc = lookup_label(instr->string_const);
        break;

    case OP_RETURN:
        if (print_instrs) 
            printf("return\n");

        check_slot(0, TRUE, FALSE);
        if (stack_slot(0).store_type != TYPE_RETURN_ADDR) {
            print_dynamic_context();
            fprintf(stderr, "top of stack doesn't hold a return address\n");
            found_error = TRUE;
        }

        next_pc = stack_slot(0).int_val;
        top_slot--;
        break;

    case OP_CALL:
        if (print_instrs) 
            printf("call %s\n", instr->string_const);

        top_slot++;
        if (stack_check_overflow())
            break;
        stack[top_slot].store_valid = TRUE;
        stack[top_slot].store_type = TYPE_RETURN_ADDR;
        stack[top_slot].int_val = next_pc;
        next_pc = lookup_label(instr->string_const);
        break;

    case OP_CALL_BUILTIN:
        if (instr->func >= FUNCOP_LAST) 
            report_error_and_exit("bad function in call");

        builtin_func = func_names[instr->func];
        if (print_instrs) 
            printf("call_builtin %s\n", builtin_func);

        switch (instr->func) {
            case FUNCOP_READ_INT:
                init_all_regs();
                if (scanf("%d", &i1) != 1) 
                    report_error_and_exit("cannot read integer");
                set_reg_int(0, i1);
                break;

            case FUNCOP_READ_REAL:
                init_all_regs();
                if (scanf("%f", &r1) != 1) 
                    report_error_and_exit("cannot read real");
                set_reg_real(0, r1);
                break;

            case FUNCOP_READ_BOOL:
                init_all_regs();
                if (scanf("%s", buf) != 1) 
                    report_error_and_exit("cannot read bool");
                if (streq(buf, "true")) 
                    set_reg_int(0, 1);
                else if (streq(buf, "false")) 
                    set_reg_int(0, 0);
                else 
                    report_error_and_exit("read invalid bool");
                break;

            case FUNCOP_READ_STRING:
                init_all_regs();
                if (scanf("%s", buf) != 1) 
                    report_error_and_exit("cannot read string");
                set_reg_string(0, strdup(buf));
                break;

            case FUNCOP_PRINT_INT:
                printf("%d", int_reg(0));
                init_all_regs();
                break;

            case FUNCOP_PRINT_REAL:
                printf("%f", real_reg(0));
                init_all_regs();
                break;

            case FUNCOP_PRINT_BOOL:
                printf("%s", int_reg(0) ? "true" : "false");
                init_all_regs();
                break;

            case FUNCOP_PRINT_STRING:
                printf("%s", string_reg(0));
                init_all_regs();
                break;

            case FUNCOP_PRINT_NEWLINE:
                printf("\n");
                init_all_regs();
                break;

            case FUNCOP_STRING_CONCAT:
                s1 = string_reg(0);
                s2 = string_reg(1);
                l = (int) strlen(s1) + (int) strlen(s2) + 1;
                s = checked_malloc(l);
                s = strcpy(s, s1);
                s = strcat(s, s2);
                init_all_regs();
                set_reg_string(0, s);
                break;

            case FUNCOP_STRING_LENGTH:
                s1 = string_reg(0);
                init_all_regs();
                set_reg_int(0, (int) strlen(s1));
                break;

            case FUNCOP_SUBSTRING:
                s1 = string_reg(0);
                i1 = int_reg(1);
                i2 = int_reg(2);
                l = (int) strlen(s1) + 1;
                s = checked_malloc(l);
                if (i1 > l) 
                    report_error_and_exit("substring: invalid start");

                strcpy(s, s1 + i1);
                l = l - i1;
                if (i2 < l) 
                    s[i2] = '\0';
                init_all_regs();
                set_reg_string(0, s);
                break;

            case FUNCOP_SQRT:
                r1 = real_reg(0);
                init_all_regs();
                set_reg_real(0, (float) sqrt(r1));
                break;

            case FUNCOP_TRUNC:
                r1 = real_reg(0);
                init_all_regs();
                set_reg_int(instr->rd, oztrunc(r1));
                break;

            case FUNCOP_ROUND:
                r1 = real_reg(0);
                init_all_regs();
                set_reg_int(instr->rd, ozround(r1));
                break;

            case FUNCOP_LAST:
                report_error_and_exit("call: invalid function");
        }

        break;

    case OP_INT_TO_REAL:
        if (print_instrs) 
            printf("int_to_real r%d, r%d\n", instr->rd, instr->rs1);

        i1 = int_reg(instr->rs1);
        set_reg_real(instr->rd, (float) i1);
        break;

    case OP_MOVE:
        if (print_instrs) 
            printf("move r%d, r%d\n", instr->rd, instr->rs1);

        set_reg_any(instr->rd, instr->rs1);
        break;

    case OP_DEBUG_REG:
        if (print_instrs) 
            printf("debug_reg r%d\n", instr->rs1);

        check_reg(instr->rs1);
        if (quiet) 
            break;

        switch (regs[instr->rs1].store_type) {
            case TYPE_INT:
                printf("register %d: %d\n", instr->rs1, int_reg(instr->rs1));
                break;

            case TYPE_REAL:
                printf("register %d: %f\n", instr->rs1, real_reg(instr->rs1));
                break;

            case TYPE_ADDRESS:
                printf("register %d: @%d\n", instr->rs1,
                    regs[instr->rs1].int_val);
                break;

            case TYPE_STRING:
                printf("register %d: %s\n",
                    instr->rs1, string_reg(instr->rs1));
                break;

            default:
                report_error_and_exit("invalid register type");
                break;
        }

        break;

    case OP_DEBUG_SLOT:
        if (print_instrs) 
            printf("debug_slot %d\n", instr->int_const);

        check_slot(instr->int_const, TRUE, TRUE);
        if (quiet) 
            break;

        switch (stack_slot(instr->int_const).store_type) {
            case TYPE_INT:
                printf("slot %d: %d\n",
                    instr->int_const, int_slot(instr->int_const));
                break;

            case TYPE_REAL:
                printf("slot %d: %f\n",
                    instr->int_const, real_slot(instr->int_const));
                break;

            case TYPE_ADDRESS:
                printf("slot %d: @%d\n",
                    instr->int_const, stack_slot(instr->int_const).int_val);
                break;

            case TYPE_STRING:
                printf("slot %d: %s\n",
                    instr->int_const, string_slot(instr->int_const));
                break;

            case TYPE_FRAME_SIZE:
                report_error_and_exit("frame size slot");
                break;

            case TYPE_RETURN_ADDR:
                report_error_and_exit("return address slot");
                break;

            default:
                report_error_and_exit("invalid slot type");
                break;
        }

        break;

    case OP_DEBUG_STACK:
        if (print_instrs) 
            printf("debug_stack\n");

        if (quiet) 
            break;

        printf("\n");
        printf("cur_frame_size = %d\n", cur_frame_size);
        for (offset = 0; offset <= top_slot; offset++) {
            if (! stack[offset].store_valid) {
                printf("offset %d is invalid\n", offset);
                continue;
            }

            switch (stack[offset].store_type) {

            case TYPE_INT:
                printf("offset %d: %d\n", offset, stack[offset].int_val);
                break;

            case TYPE_REAL:
                printf("offset %d: %f\n", offset, stack[offset].real_val);
                break;

            case TYPE_ADDRESS:
                printf("offset %d: @%d\n", offset, stack[offset].int_val);
                break;

            case TYPE_STRING:
                printf("offset %d: %s\n", offset, stack[offset].string_val);
                break;

            case TYPE_FRAME_SIZE:
                printf("offset %d: frame size %d\n",
                    offset, stack[offset].int_val);
                break;

            case TYPE_RETURN_ADDR:
                printf("offset %d: return address %d\n",
                    offset, stack[offset].int_val);
                break;

            default:
                report_error_and_exit("invalid slot type");
                break;
            }
        }

        printf("\n");
        break;

    case OP_HALT:
        if (print_instrs) 
            printf("halt\n");

        halted = TRUE;
        break;

    default:
        report_internal_error_and_exit("unknown opcode");
        break;
    }
}

/***********************************************************************/

static void *
checked_malloc(int num_bytes) {

    void    *addr;

    addr = malloc((size_t) num_bytes);
    if (addr == NULL) 
        report_internal_error_and_exit("out of memory");
    return addr;
}

static char *
make_string_const(char *orig) {

    int len;
    char    *target;
    int to;
    int from;

    len = (int) strlen(orig);
    target = checked_malloc(len);

    to = 0;
    for (from = 1; from < len - 1; from++) {
        if (orig[from] == '\\' && orig[from+1] == 'n') {
            from++;
            target[to] = '\n';
        } else 
            target[to] = orig[from];
        to++;
    }

    target[to] = '\0';
    return target;
}

static int
oztrunc(float f) {

    int i = (int) f;

    if ((float) i > f) 
        i--;
    return i;
}

static int
ozround(float f) {

    int i;

    i = oztrunc(f);
    if ((f - (float) i) > (((float) i+1) -f)) 
        i++;
    return i;
}

/***********************************************************************/

void
report_error_and_exit(const char *msg) {

    (void) fflush(NULL);
    fprintf(stderr, "%s\n", msg);
    exit(EXIT_FAILURE);
}

void
report_perror_and_exit(const char *msg) {

    (void) fflush(NULL);
    perror(msg);
    exit(EXIT_FAILURE);
}

void
report_internal_error_and_exit(const char *msg) {

    (void) fflush(NULL);
    fprintf(stderr, "simulator internal error: %s\n", msg);
    exit(EXIT_FAILURE);
}

/***********************************************************************/

Instr *
record_instr(void) {

    int this_instr;

    if (next_instr >= MAX_NUM_INSTRS) 
        report_error_and_exit("program contains too many instructions");

    this_instr = next_instr;
    next_instr++;
    return &instrs[this_instr];
}

static bool
check_instr(int which) {

    if (0 <= which && which < next_instr)
        return TRUE;
    else
        return FALSE;
}

/***********************************************************************/

void
record_label(char *name) {

    int     i;
    char    buf[256];

    for (i = 0; i < next_label; i++) {
        if (streq(name, labels[i].label_name)) {
            (int) snprintf(buf, 256, "duplicate label %s", name);
            report_error_and_exit(buf);
        }
    }

    if (! (next_label < MAX_NUM_LABELS)) 
        report_error_and_exit("program contains too many labels");

    labels[next_label].label_name = strdup(name);
    labels[next_label].label_pc = next_instr;
    next_label++;
}

static int
lookup_label(char *name) {

    int i;

    for (i = 0; i < next_label; i++) 
        if (streq(name, labels[i].label_name)) 
            return labels[i].label_pc;

    print_dynamic_context();
    fprintf(stderr, "reference to undefined label %s\n", name);
    found_error = TRUE;
    return -1;
}

/***********************************************************************/

static void
print_dynamic_context(void) {

    (void) fflush(NULL);
    fprintf(stderr, "at instruction %d, pc %d: ", cur_instr, pc);
}

static void
init_all_regs(void) {

    int i;

    for (i = 0; i < NUMBER_REGS; i++) 
        regs[i].store_valid = FALSE;
}

static void
init_stack(void) {

    int i;

    for (i = 0; i < STACK_SIZE; i++) 
        stack[i].store_valid = FALSE;
}

static bool
stack_check_overflow(void) {

    if (top_slot >= STACK_SIZE)
    {
        print_dynamic_context();
        fprintf(stderr, "stack overflow\n");
        found_error = TRUE;
        return TRUE;
    }
    return FALSE;
}

static void
set_reg_any(int reg, int src) {

    if (! regs[src].store_valid) {
        print_dynamic_context();
        fprintf(stderr, "register %d is not valid\n", src);
        found_error = TRUE;
        return;
    }

    regs[reg].store_valid = TRUE;
    regs[reg].store_type = regs[src].store_type;
    regs[reg].int_val = regs[src].int_val;
    regs[reg].real_val = regs[src].real_val;
    regs[reg].string_val = regs[src].string_val;
}

static void
set_reg_int(int reg, int value) {

    regs[reg].store_valid = TRUE;
    regs[reg].store_type = TYPE_INT;
    regs[reg].int_val = value;
}

static void
set_reg_real(int reg, float value) {

    regs[reg].store_valid = TRUE;
    regs[reg].store_type = TYPE_REAL;
    regs[reg].real_val = value;
}

static void
set_reg_string(int reg, char *value) {

    regs[reg].store_valid = TRUE;
    regs[reg].store_type = TYPE_STRING;
    regs[reg].string_val = value;
}

static void
set_reg_addr(int reg, int offset) {

    regs[reg].store_valid = TRUE;
    regs[reg].store_type = TYPE_ADDRESS;
    regs[reg].int_val = offset;
}

static void
check_reg(int reg) {

    if (! (0 <= reg && reg < NUMBER_REGS)) {
        print_dynamic_context();
        fprintf(stderr, "register %d does not exist\n", reg);
        found_error = TRUE;
    }

    if (! regs[reg].store_valid) {
        print_dynamic_context();
        fprintf(stderr, "register %d is not valid\n", reg);
        found_error = TRUE;
    }
}

static void
check_slot(int slot, bool check_valid, bool check_frame) {

    if (check_frame && ! (0 <= slot && slot < cur_frame_size)) {
        print_dynamic_context();
        fprintf(stderr, 
            "the current stack frame does not have slot %d\n", slot);
        found_error = TRUE;
    }

    if (check_valid && ! stack_slot(slot).store_valid) {
        print_dynamic_context();
        fprintf(stderr, 
            "slot %d does not contain valid data\n", slot);
        found_error = TRUE;
    }
}

static void
check_offset(int offset, bool check_valid, bool check_frame) {

    if (check_frame && ! (0 <= offset && offset <= top_slot)) {
        print_dynamic_context();
        fprintf(stderr, 
            "address @%d is not in any current stack frame\n", offset);
        found_error = TRUE;
    }

    if (check_valid && ! stack[offset].store_valid) {
        print_dynamic_context();
        fprintf(stderr, 
            "address @%d does not contain valid data\n", offset);
        found_error = TRUE;
    }
}

static int
int_reg(int reg) {

    check_reg(reg);
    if (regs[reg].store_type != TYPE_INT) {
        print_dynamic_context();
        fprintf(stderr, "register %d doesn't contain an int\n", reg);
        found_error = TRUE;
    }

    return regs[reg].int_val;
}

static float
real_reg(int reg) {

    check_reg(reg);
    if (regs[reg].store_type != TYPE_REAL) {
        print_dynamic_context();
        fprintf(stderr, "register %d doesn't contain a real\n", reg);
        found_error = TRUE;
    }

    return regs[reg].real_val;
}

static char *
string_reg(int reg) {

    check_reg(reg);
    if (regs[reg].store_type != TYPE_STRING) {
        print_dynamic_context();
        fprintf(stderr, "register %d doesn't contain a string\n", reg);
        found_error = TRUE;
    }

    return regs[reg].string_val;
}

static int
addr_reg(int reg) {

    check_reg(reg);
    if (regs[reg].store_type != TYPE_ADDRESS) {
        print_dynamic_context();
        fprintf(stderr, "register %d doesn't contain an address\n", reg);
        found_error = TRUE;
    }

    return regs[reg].int_val;
}

static int
int_slot(int slot) {

    check_slot(slot, TRUE, TRUE);
    if (stack_slot(slot).store_type != TYPE_INT) {
        print_dynamic_context();
        fprintf(stderr, "slot %d doesn't contain an int\n", slot);
        found_error = TRUE;
    }

    return stack_slot(slot).int_val;
}

static float
real_slot(int slot) {

    check_slot(slot, TRUE, TRUE);
    if (stack_slot(slot).store_type != TYPE_REAL) {
        print_dynamic_context();
        fprintf(stderr, "slot %d doesn't contain a real\n", slot);
        found_error = TRUE;
    }

    return stack_slot(slot).real_val;
}

static char *
string_slot(int slot) {

    check_slot(slot, TRUE, TRUE);
    if (stack_slot(slot).store_type != TYPE_STRING) {
        print_dynamic_context();
        fprintf(stderr, "slot %d doesn't contain a string\n", slot);
        found_error = TRUE;
    }

    return stack_slot(slot).string_val;
}

/***********************************************************************/
