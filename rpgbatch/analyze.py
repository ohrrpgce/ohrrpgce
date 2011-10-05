#!/usr/bin/env python
import sys
import time
import cPickle as pickle
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.dtypes import dt

if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit("Expected pickled file")
    pickled = sys.argv[1]
else:
    pickled = 'gamedata.bin'

with open(pickled, 'rb') as f:
    d = pickle.load(f)

rpgidx = d['rpgidx']
gen = d['gen']
mas = d['mas']
fnt = d['fnt']
del d
