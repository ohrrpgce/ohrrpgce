#!/usr/bin/env python3

import argparse
import os
from os import path
import sys

sys.path.insert(0,
    path.join(path.dirname(sys.argv[0]), "hs_lib")
)

import hs_tld
import hs_file

# command line options
main_args = None

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description = "HSpeak compiler")
    parser.add_argument("-d", type = int, default = 0, help = "Debug")
    parser.add_argument("hss", type = str, help = "Input hss file name")

    main_args = parser.parse_args()
    hs_tld.main_args = main_args

    fn = main_args.hss

    if not path.isfile(fn):
        print(fn, "not found")
        exit()

    hs_file.hs_begin(fn)
    hs_tld.parse_hss(fn)
    hs_file.hs_end()
