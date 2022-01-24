#!/bin/python3

import time
import os
import sys
import pathlib
import subprocess
from typing import List
from colorama import Fore, Style
from dataclasses import dataclass

LILY_EXT = "lily"

def is_lily_ext(file: str) -> bool:
    if pathlib.Path(file).suffix == ".lily": return True
    return False

def run_command(cmd, **cmd_args):
    return subprocess.run(cmd, **cmd_args)

def test_to_str(is_ok) -> str:
    if is_ok: return Fore.GREEN + "ok" + Style.RESET_ALL
    return Fore.RED + "failed" + Style.RESET_ALL

class Test:
    file: str
    is_ok: bool

    def __init__(self, file, is_ok):
        self.file = file
        self.is_ok = is_ok
    
    def __str__(self):
        s = ""
        s += file + " ... " + test_to_str(self.is_ok)
        return s

def run_test(file: str, nb_tests: int, count: int) -> bool:
    is_ok = True
    command = run_command(["./_build/default/src/lily.exe", "run", file], capture_output=True)

    if command.returncode == 1:
        is_ok = False
        print(str(count) + "/" + str(nb_tests) + " test " + str(Test(file, is_ok)))
        return is_ok
    
    print(str(count) + "/" + str(nb_tests) + " test " + str(Test(file, is_ok)))
    return is_ok

@dataclass
class TestResults:
    success: int
    failed: int

class Tests:
    results: TestResults
    time: float

    def __str__(self):
        s = ""
        s += "\n" + str(self.results.success + self.results.failed) + " tests run in " + str(round(self.time, 3)) + " seconds." + "\n"
        s += Fore.RED + str(self.results.failed) + " failed" + Style.RESET_ALL + Fore.GREEN + " (" + str(self.results.success) + " tests passed)" + Style.RESET_ALL
        return s
    
    def __init__(self, results, time):
        self.results = results
        self.time = time
        
def get_files_in_dirs(dirs: List[str]) -> List[str]:
    file_l = []
    for dir in dirs:
        if not os.path.isdir(dir):
            print("error: the directory doesnt\'t exists: `%s`" % (dir))
            exit(1)
        for _, _, files in os.walk(dir):
            for file in files:
                file_l.append(dir + '/' + file)
    return file_l

if __name__ == "__main__":
    run_command("make")
    files = get_files_in_dirs(["tests/programs"])
    nb_tests = len(list(filter(lambda f: is_lily_ext(f), files)))
    is_ok = []
    start = time.time()

    count = 1
    for file in files:
        if not is_lily_ext(file): continue
        else:
            is_ok.append(run_test(file, nb_tests, count))
            count += 1
    
    end = time.time()

    ok_count = len(list(filter(lambda x: x, is_ok)))
    err_count = len(is_ok) - ok_count

    tests_t = Tests(TestResults(ok_count, err_count), end-start)
    print(tests_t)
