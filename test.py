from typing import *
import os
import subprocess
import difflib

from tqdm import tqdm #comment this line if in test env

def get_all_test_case(folder: str) -> List[str]:
    ans = []
    for file in os.listdir("./testdata/" + folder):
        if file.endswith(".goat") and file[:2] in ['c-', 'w-']:
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


def test_prettier() -> None:
    print(" Testing: prettier")
    print("------------------")
    test_cases = get_all_test_case("prettier")
    
    #for i, case in enumerate(test_cases):
    #    print("({} / {}) {}:".format(i+1, len(test_cases), case))
    #    run_test_case(case)
    for i in tqdm(range(len(test_cases))): # comment these lines
        run_test_case(test_cases[i])

def build() -> None:
    print(" Building")
    print("------------------")
    cp = subprocess.run(["make"])
    if cp.returncode != 0:
        print()
        print("Build Error !!!!")
        exit(1)


def main() -> None:
    build()
    test_prettier()

  
if __name__== "__main__":
    main()