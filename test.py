from typing import *
import os
import sys
import subprocess
import difflib
import re

from tqdm import tqdm #comment this line if in test env

test_single = False

############# Helper #################

def printError() -> None:
    print("\033[0;31m" + "Error: " + "\033[0m", end='')

def printWarning() -> None:
    print("\033[1;33m" + "Warning: " + "\033[0m", end='')

######################################

def get_all_test_case(folder: str) -> List[str]:
    ans = []
    for file in os.listdir("./testdata/" + folder):
        # c - correct, w - wrong syntax, s - semantic, r - runtime
        if file.endswith(".gt") and file[:2] in ['c-', 'w-', 's-', 'r-']:
            ans.append(os.path.join(folder, file))
    return ans

def run_test_case(input_file: str) -> None:
    input_file = "./testdata/" + input_file
    file_name = os.path.splitext(os.path.basename(input_file))[0]
    file_path = os.path.dirname(input_file)

    # run process
    cp = subprocess.run(["./Goat", "-p", input_file], stdout=subprocess.PIPE)
    
    diff_cp = None

    sample_output = os.path.join(file_path, file_name + '.out')
    diff_cp = subprocess.run(["diff", sample_output, "-"], input=cp.stdout, stdout=subprocess.PIPE)
    if diff_cp.returncode == 0:
        # print("Pass")
        return

    print("\033[0;31m" + "Error: " + "\033[0m" + file_name)
    print("Fail !!!!")
    print("Return code:", cp.returncode)
    print("-----------------")
    if diff_cp:
        print("Diff:")
        print((diff_cp.stdout).decode("utf-8"))
    else:
        print("Stdout: ")
        print(cp.stdout)
        print()
    exit(1)

def run_compiler_test_case(input_file: str) -> None:
    input_file = "./testdata/" + input_file
    file_name = os.path.splitext(os.path.basename(input_file))[0]
    file_path = os.path.dirname(input_file)
    goat_output_path = os.path.join(file_path, file_name + '.oz')
    stdin_path = os.path.join(file_path, file_name + '.in')
    sample_output = os.path.join(file_path, file_name + '.out')
    prefix = file_name[:2]

    if test_single:
        print("Checking " + file_name)

    with open(goat_output_path, 'w') as fp:
        cp = subprocess.run(["./Goat", input_file], stdout=fp, stderr=subprocess.PIPE)

    if cp.returncode == 1:
        printError()
        print("Invalid command line arguments")
        exit(1)
    
    if prefix == 'w-':
        if cp.returncode == 2:
            return
        else:
            printError()
            print("Expected to be syntax error with exit code 2")
            exit(1)
    
    if prefix == 's-':
        if cp.returncode == 3:
            return
        else:
            printError()
            print("Expected to be semantic error with exit code 3")
            exit(1)

    
    if cp.returncode == 0:
        is_stdin_exist = os.path.isfile(stdin_path)
        oz_location = './oz'

        if is_stdin_exist:
            with open(stdin_path, 'rb') as fin:
                oz_cp = subprocess.run([oz_location, goat_output_path], input=fin.read(), stdout=subprocess.PIPE)
        else:
            try:
                oz_cp = subprocess.run([oz_location, goat_output_path], stdout=subprocess.PIPE, timeout=30)
            except TimeoutExpired:
                printWarning()
                print(filename + " has no input file or it runs for too long")

        
        if prefix == 'r-':
            if oz_cp.returncode != 0:
                return
            else:
                printError()
                print("File: " + file_name)
                print("Expected to be a run-time error. However having output:")
                print((oz_cp.stdout).decode("utf-8"))
                exit(1)

        if oz_cp.returncode == 0:
            if test_single:
                print("-----")
                print("Oz emulator output:")
                print("-----")
                print((oz_cp.stdout).decode("utf-8"))

            is_out_exist = os.path.isfile(sample_output)

            if not is_out_exist:
                printWarning()
                print(file_name + " does not have a sample output file")
                return

            diff_cp = subprocess.run(["diff", sample_output, "-"], input=oz_cp.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            if diff_cp.returncode == 0:
                return

            printError()
            print("File: " + file_name)
            print("Return code:", cp.returncode)
            print("-----------------")
            if diff_cp:
                print("Diff:")
                print((diff_cp.stdout).decode("utf-8"))
            else:
                print("Stdout: ")
                print(cp.stdout)
                print()
            exit(1)
        
        printError()
        print(file_name + ": running Oz emulator failure")
        exit(1)
    
    printError()
    print(file_name + ": compilation exits with error code", cp.returncode)
    with open(goat_output_path, 'r') as f:
        print(f.read())
    exit(1)

def test_prettier() -> None:
    print("\n==================")
    print(" Testing: prettier")
    print("------------------")
    test_cases = get_all_test_case("prettier")
    
    #for i, case in enumerate(test_cases):
    #    print("({} / {}) {}:".format(i+1, len(test_cases), case))
    #    run_test_case(case)
    for i in tqdm(range(len(test_cases))): # comment these lines
        run_test_case(test_cases[i])

def test_compiler() -> None:
    global test_single
    print("\n==================")
    print(" Testing: compiler")
    print("------------------")

    args = sys.argv[1:]

    if len(args) == 1:
        rst = re.search("(?<=testdata/)compiler/.*\.gt", args[0])
        if rst is None:
            printError()
            print("Invalid Goat program path. Should be something like 'testdata/compiler/example.gt")
            exit(1)
        test_single = True
        test_cases = [rst.group(0)]
        print("Using test case: " + test_cases[0])
    elif len(args) == 0:
        print("Using all of the test cases")
        test_cases = get_all_test_case("compiler")
    else:
        printError()
        print("Invalid arguments.")
        print("Usage: python3 test.py")
        print("Usage: python3 test.py testdata/compiler/testcase.gt")
        exit(1)

    if test_single:
        run_compiler_test_case(test_cases[0])
    else:
        for i in tqdm(range(len(test_cases))): # comment these lines
            run_compiler_test_case(test_cases[i])

def build() -> None:
    print("\n==================")
    print(" Building Goat")
    print("------------------")
    cp = subprocess.run(["make"])
    if cp.returncode != 0:
        printError()
        print("Build failed !!!!")
        exit(1)
    else:
        print("Build successfully")

def buildOz() -> None:
    print("\n==================")
    print(" Building Oz Emulator")
    print("------------------")
    cp = subprocess.run(["make"], stderr=subprocess.DEVNULL, cwd="./resources/oz")
    if cp.returncode != 0:
        printError()
        print("Build failed !!!!")
        exit(1)
    else:
        print("Build successfully")

def main() -> None:
    print("Using Error code:\n\t1 -> Command line arguments error\n\t2 -> Synatax error\n\t3 -> Semantic error\n")

    build()
    # test_prettier()

    # buildOz()
    test_compiler()

  
if __name__== "__main__":
    main()
