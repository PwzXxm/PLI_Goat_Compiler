from typing import *
import os
import subprocess
import difflib

from tqdm import tqdm #comment this line if in test env

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
    is_correct = file_name[:2] == 'c-'

    # run process
    cp = subprocess.run(["./Goat", "-p", input_file], stdout=subprocess.PIPE)
    
    diff_cp = None

    sample_output = os.path.join(file_path, file_name + '.out')
    diff_cp = subprocess.run(["diff", sample_output, "-"], input=cp.stdout, stdout=subprocess.PIPE)
    if diff_cp.returncode == 0:
        # print("Pass")
        return

    print("File: " + "\033[0;31m" + file_name + "\033[0m") # comment this line
    print("Fail !!!!")
    print("Is correct case:", is_correct)
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
    input_file = "./compiler/" + input_file
    file_name = os.path.splitext(os.path.basename(input_file))[0]
    file_path = os.path.dirname(input_file)
    goat_output_path = os.path.join(file_path, file_name + '.oz')
    stdin_path = os.path.join(file_path, file_name + '.in')
    oz_output_path = os.path.join(file_path, file_name + '.in')
    prefix = file_name[:2]

    with open(goat_output_path, 'w') as fp:
        cp = subprocess.run(["./Goat", input_file], stdout=fp)

    if cp.returncode == 1:
        print("Invalid command line arguments")
        exit(1)
    
    if cp.returncode == 2 and prefix == 'w-':
        return
    
    if cp.returncode == 3 and prefix == 's-':
        return
    
    if cp.returncode == 0:
        is_stdin_file_path = os.path.isfile(stdin_path)
        oz_location = './resources/oz/oz'

        with open(oz_output_path, 'w') as fout:
            if is_stdin_file_path:
                with open(stdin_path, 'w') as fin:
                    oz_cp = subprocess.run([oz_location], stdin=fin, stdout=fout)
            else:
                oz_cp = subprocess.run([oz_location], stdout=file_name)
        
        if oz_cp.returncode != 0 and prefix == 'r-':
            return

        if oz_cp.returncode == 0:
            # maybe checkout output?
            return
        
        print("Running Oz emulator failure")
        exit(1)
    
    print("Compilation exits with error code" + cp.returncode)
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
    print("\n==================")
    print(" Testing: compiler")
    print("------------------")
    test_cases = get_all_test_case("compiler")

    for i in tqdm(range(len(test_cases))): # comment these lines
        run_test_case(test_cases[i])

def build() -> None:
    print("\n==================")
    print(" Building Goat")
    print("------------------")
    cp = subprocess.run(["make"])
    if cp.returncode != 0:
        print()
        print("Build Error !!!!")
        exit(1)
    else:
        print("Build successfully")

def buildOz() -> None:
    print("\n==================")
    print(" Building Oz Emulator")
    print("------------------")
    cp = subprocess.run(["make"], stderr=subprocess.DEVNULL, cwd="./resources/oz")
    if cp.returncode != 0:
        print()
        print("Build Error !!!!")
        exit(1)
    else:
        print("Build successfully")

def main() -> None:
    print("Using Error code:\n\t1 -> Command line arguments error\n\t2 -> Synatax error\n\t3 -> Semantic error\n")

    build()
    # test_prettier()

    buildOz()
    # test_compiler()

  
if __name__== "__main__":
    main()