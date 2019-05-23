proc_main:
  # Prologue
    push_stack_frame 6
  # Passing parameters
  # Initialise Declaration
    int_const r0, 0
    real_const r1, 0.0
    store 0, r0         # a
    store 1, r0         # b
    store 2, r0         # c
    store 3, r0         # d
    store 4, r0         # e
    store 5, r0         # f
  # Compile Statements
  # Assign (SBaseVar "a") (IntConst 1)
    int_const r0, 1
    store 0, r0
  # Assign (SBaseVar "b") (IntConst 10)
    int_const r0, 10
    store 1, r0
  # Assign (SBaseVar "c") (IntConst 100)
    int_const r0, 100
    store 2, r0
  # Assign (SBaseVar "d") (IntConst 1000)
    int_const r0, 1000
    store 3, r0
  # Assign (SBaseVar "d") (IntConst 1000)
    int_const r0, 1000
    store 3, r0
  # Assign (SBaseVar "e") (IntConst 10000)
    int_const r0, 10000
    store 4, r0
  # Assign (SBaseVar "f") (IntConst 20000)
    int_const r0, 20000
    store 5, r0
  # While (And (And (And (And (And (LessEqual (Id (SBaseVar "a")) (IntConst 100)) (LessEqual (Id (SBaseVar "b")) (IntConst 110))) (LessEqual (Id (SBaseVar "c")) (IntConst 200))) (LessEqual (Id (SBaseVar "d")) (IntConst 1100))) (LessEqual (Id (SBaseVar "e")) (IntConst 10100))) (LessEqual (Id (SBaseVar "f")) (IntConst 20100))) [Assign (SBaseVar "a") (Add (Id (SBaseVar "a")) (IntConst 1)),Assign (SBaseVar "b") (Add (Id (SBaseVar "b")) (IntConst 1)),Assign (SBaseVar "c") (Add (Id (SBaseVar "c")) (IntConst 1)),Assign (SBaseVar "d") (Add (Id (SBaseVar "d")) (IntConst 1)),Assign (SBaseVar "e") (Add (Id (SBaseVar "e")) (IntConst 1)),Assign (SBaseVar "f") (Add (Id (SBaseVar "f")) (IntConst 1))]
label_0:
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    load r1, 1
    int_const r2, 110
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 2
    int_const r2, 200
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 3
    int_const r2, 1100
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 4
    int_const r2, 10100
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 5
    int_const r2, 20100
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    branch_on_false r0, label_1
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    load r1, 1
    int_const r2, 1
    add_int r1, r1, r2
    store 1, r1
    load r2, 2
    int_const r3, 1
    add_int r2, r2, r3
    store 2, r2
    load r3, 3
    int_const r4, 1
    add_int r3, r3, r4
    store 3, r3
    load r4, 4
    int_const r5, 1
    add_int r4, r4, r5
    store 4, r4
    load r5, 5
    int_const r6, 1
    add_int r5, r5, r6
    store 5, r5
    branch_uncond label_0
label_1:
  # While (Or (Or (LessEqual (Id (SBaseVar "a")) (IntConst 100)) (And (LessEqual (Id (SBaseVar "b")) (IntConst 110)) (LessEqual (Id (SBaseVar "c")) (IntConst 200)))) (And (And (LessEqual (Id (SBaseVar "d")) (IntConst 1100)) (LessEqual (Id (SBaseVar "e")) (IntConst 10100))) (LessEqual (Id (SBaseVar "f")) (IntConst 20100)))) [Assign (SBaseVar "a") (Add (Id (SBaseVar "a")) (IntConst 1)),Assign (SBaseVar "b") (Add (Id (SBaseVar "b")) (IntConst 1)),Assign (SBaseVar "c") (Add (Id (SBaseVar "c")) (IntConst 1)),Assign (SBaseVar "d") (Add (Id (SBaseVar "d")) (IntConst 1)),Assign (SBaseVar "e") (Add (Id (SBaseVar "e")) (IntConst 1)),Assign (SBaseVar "f") (Add (Id (SBaseVar "f")) (IntConst 1))]
label_2:
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    load r1, 1
    int_const r2, 110
    cmp_le_int r1, r1, r2
    load r2, 2
    int_const r3, 200
    cmp_le_int r2, r2, r3
    and r1, r1, r2
    or r0, r0, r1
    load r1, 3
    int_const r2, 1100
    cmp_le_int r1, r1, r2
    load r2, 4
    int_const r3, 10100
    cmp_le_int r2, r2, r3
    and r1, r1, r2
    load r2, 5
    int_const r3, 20100
    cmp_le_int r2, r2, r3
    and r1, r1, r2
    or r0, r0, r1
    branch_on_false r0, label_3
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    load r1, 1
    int_const r2, 1
    add_int r1, r1, r2
    store 1, r1
    load r2, 2
    int_const r3, 1
    add_int r2, r2, r3
    store 2, r2
    load r3, 3
    int_const r4, 1
    add_int r3, r3, r4
    store 3, r3
    load r4, 4
    int_const r5, 1
    add_int r4, r4, r5
    store 4, r4
    load r5, 5
    int_const r6, 1
    add_int r5, r5, r6
    store 5, r5
    branch_uncond label_2
label_3:
  # If (And (And (And (And (And (LessEqual (Id (SBaseVar "a")) (IntConst 100)) (LessEqual (Id (SBaseVar "b")) (IntConst 110))) (LessEqual (Id (SBaseVar "c")) (IntConst 200))) (LessEqual (Id (SBaseVar "d")) (IntConst 1100))) (LessEqual (Id (SBaseVar "e")) (IntConst 10100))) (LessEqual (Id (SBaseVar "f")) (IntConst 20100))) [Assign (SBaseVar "a") (Add (Id (SBaseVar "a")) (IntConst 1)),Assign (SBaseVar "b") (Add (Id (SBaseVar "b")) (IntConst 1)),Assign (SBaseVar "c") (Add (Id (SBaseVar "c")) (IntConst 1)),Assign (SBaseVar "d") (Add (Id (SBaseVar "d")) (IntConst 1)),Assign (SBaseVar "e") (Add (Id (SBaseVar "e")) (IntConst 1)),Assign (SBaseVar "f") (Add (Id (SBaseVar "f")) (IntConst 1))] []
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    load r1, 1
    int_const r2, 110
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 2
    int_const r2, 200
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 3
    int_const r2, 1100
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 4
    int_const r2, 10100
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    load r1, 5
    int_const r2, 20100
    cmp_le_int r1, r1, r2
    and r0, r0, r1
    branch_on_false r0, label_4
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    load r1, 1
    int_const r2, 1
    add_int r1, r1, r2
    store 1, r1
    load r2, 2
    int_const r3, 1
    add_int r2, r2, r3
    store 2, r2
    load r3, 3
    int_const r4, 1
    add_int r3, r3, r4
    store 3, r3
    load r4, 4
    int_const r5, 1
    add_int r4, r4, r5
    store 4, r4
    load r5, 5
    int_const r6, 1
    add_int r5, r5, r6
    store 5, r5
label_4:
  # If (Or (Or (LessEqual (Id (SBaseVar "a")) (IntConst 100)) (And (LessEqual (Id (SBaseVar "b")) (IntConst 110)) (LessEqual (Id (SBaseVar "c")) (IntConst 200)))) (And (And (LessEqual (Id (SBaseVar "d")) (IntConst 1100)) (LessEqual (Id (SBaseVar "e")) (IntConst 10100))) (LessEqual (Id (SBaseVar "f")) (IntConst 20100)))) [Assign (SBaseVar "a") (Add (Id (SBaseVar "a")) (IntConst 1)),Assign (SBaseVar "b") (Add (Id (SBaseVar "b")) (IntConst 1)),Assign (SBaseVar "c") (Add (Id (SBaseVar "c")) (IntConst 1)),Assign (SBaseVar "d") (Add (Id (SBaseVar "d")) (IntConst 1)),Assign (SBaseVar "e") (Add (Id (SBaseVar "e")) (IntConst 1)),Assign (SBaseVar "f") (Add (Id (SBaseVar "f")) (IntConst 1))] []
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    load r1, 1
    int_const r2, 110
    cmp_le_int r1, r1, r2
    load r2, 2
    int_const r3, 200
    cmp_le_int r2, r2, r3
    and r1, r1, r2
    or r0, r0, r1
    load r1, 3
    int_const r2, 1100
    cmp_le_int r1, r1, r2
    load r2, 4
    int_const r3, 10100
    cmp_le_int r2, r2, r3
    and r1, r1, r2
    load r2, 5
    int_const r3, 20100
    cmp_le_int r2, r2, r3
    and r1, r1, r2
    or r0, r0, r1
    branch_on_false r0, label_5
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    load r1, 1
    int_const r2, 1
    add_int r1, r1, r2
    store 1, r1
    load r2, 2
    int_const r3, 1
    add_int r2, r2, r3
    store 2, r2
    load r3, 3
    int_const r4, 1
    add_int r3, r3, r4
    store 3, r3
    load r4, 4
    int_const r5, 1
    add_int r4, r4, r5
    store 4, r4
    load r5, 5
    int_const r6, 1
    add_int r5, r5, r6
    store 5, r5
label_5:
  # Epilogue
    pop_stack_frame 6
    return
