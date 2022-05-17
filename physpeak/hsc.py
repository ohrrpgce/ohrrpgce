#!/usr/bin/env python3

import argparse
import os
from os import path
import sys
import hslib.tld
import hslib.hsfile

# command line options
main_args = None

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description = "HSpeak compiler")
    parser.add_argument("-d", type = int, default = 0, help = "Debug")
    parser.add_argument("-v", type = bool, default = False, help = "Verbose")
    parser.add_argument("hss", type = str, help = "Input hss file name")

    main_args = parser.parse_args()
    hslib.tld.verbose = main_args.v

    fn = main_args.hss

    if path.splitext(fn)[1] == ".hs":
        print("error: .hs extension is reserved. Use .hss or .txt")
        exit()

    if not path.isfile(fn):
        print(fn, "not found")
        exit()

    hslib.hsfile.hs_begin(fn)
    hslib.tld.parse_hss(fn)
    hslib.hsfile.hs_end()
