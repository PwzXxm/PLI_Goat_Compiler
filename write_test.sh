#!/bin/sh
# Use this script to write test case
# To run: ./write_test test_name
# e.g.
#   ./write_test w-no-global-variable-float
#   ./write_test c-empty 

testdata_path="./testdata/compiler/"

vi_sth() {
    # change vi to your favourite text editor
    eval "vim ${testdata_path}$1$2"
}

if [ $# -eq 1 ]
then
    file=$1
    cp_c="cat ${testdata_path}c-basic.gt > ${testdata_path}${file}.gt"
    #run_c="./Goat ${testdata_path}${file}.gt > ${testdata_path}${file}.out"

    # Make Goat
    # eval "make"
    # wait $!

    # add basic template to test file
    eval $cp_c

    # modify .goat test file
    wait $!
    vi_sth $file ".gt"

    wait $!
    vi_sth $file ".out"

    wait $!
    gt_file=${testdata_path}$1.gt
    python3 test.py ${gt_file}
else
    echo "Usage: ./write_test TEST_CASE_NAME"
fi
