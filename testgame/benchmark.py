#!/usr/bin/env python3
#
# This is a port of a few of the microbenchmarks in benchmark.hss to Python for comparison
#
# Runs under MicroPython too if the from __future__ line is commented out
from __future__ import print_function
try:
    import timeit
except:
    # For MicroPython
    import time

    class timeit:
        @classmethod
        def repeat(cls, script, repeat, number=1):
            ret = []
            for i in range(repeat):
                timing = time.time()
                script()
                timing = time.time() - timing
                ret.append(timing)
            return ret


NUM_RUNS = 100
MICRO_LOOPCOUNT = 1000

########################################################################

def benchmark_while_loop():
    i = MICRO_LOOPCOUNT
    while i:
        i -= 1

def benchmark_continue_loop():
    i = MICRO_LOOPCOUNT
    while True:
        i -= 1
        if i:
            continue
        break

def benchmark_addition():
  x, y = 0, 0
  for i in range(MICRO_LOOPCOUNT // 10 + 1):
      x + y
      x + y
      x + y
      x + y
      x + y
      x + y
      x + y
      x + y
      x + y
      x + y

def empty_script():
    pass

def empty_multiarg_script(a, b, c, d):
    pass

def benchmark_call_script():
  for i in range(MICRO_LOOPCOUNT // 10 + 1):
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()
      empty_script()

def benchmark_call_multiarg_script():
  for i in range(MICRO_LOOPCOUNT // 10 + 1):
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)
      empty_multiarg_script (i, i, i, i)

def fibonacci(n):
  if n <= 1:
      return 1
  else:
      return fibonacci (n - 1) + fibonacci (n - 2)

def benchmark_recursive_fibonacci():
    fibonacci (14)

# a and b are 16 bit fixed point numbers, where b >= 1.0
# Return a*b
def FixedMul(a, b):
    if a >= 0:
        return (a // 0x10000) * b + ((a & 0xffff) * b + 0x8000) // 0x10000
    else:
        # fixme: not sure the rounding here is correct
        return (a // 0x10000) * b - ((-a & 0xffff) * b + 0x8000) // 0x10000

def benchmark_fixedmul():
    total = 0
    for i in range(-100000, 100000, 8000):
        for j in range(-100000, 100000, 8000):
            total += FixedMul(i, j)
    return total

#print("FixedMul ", benchmark_fixedmul())

#----

def benchmark_string_iter():
    for i in range(8):
        s = "The quick onyx goblin jumps over the lazy dwarf. Bright vixens jump, lazy fowl quack. Amazingly few discotheques provide jukeboxes."
        words = 1
        hsh = 0
        for ch in s:
            ch = ord(ch)
            if ch == 32: words += 1
            hsh += words * ch
        #if i == 0: print("HASH", hsh)

#----

def crappy_sqrt(fi):
  approx = -1
  if fi >= 32581:
      return 181
  else:

        if fi < 100:
            start = 0
            divi = start ** 2
        else:
            if fi >= 22500:
                start = 150
            else:
                if fi >= 14400:
                    start = 120
                else:
                    if fi >= 8100:
                        start = 90
                    else:
                        if fi >= 4225:
                            start = 65
                        else:
                            if fi >= 1600:
                                start = 40
                            else:
                                if fi >= 900:
                                    start = 30
                                else:
                                    if fi >= 400:
                                        start = 20
                                    else:
                                        start = 10
            divi = start ** 2
            if fi // 3 > divi // 2:
                start = (start // 5) * 6
                divi = start ** 2
            if fi // 4 > divi // 3:
                start = (start // 7) * 8
                divi = start ** 2
        while approx == -1:
            if divi >= fi:
                approx = start
            else:
                start += 1
                divi = start ** 2
        if divi == fi:
            return approx
        else:
            if (divi - approx) == fi:
                return approx - 1
            else:
                return fi // approx + 1

# Test flow control
def benchmark_crappy_sqrt():
    for i in range(0, 81):
        crappy_sqrt(i)

########################################################################

def run_benchmark(script, loops):
    times = timeit.repeat(script, repeat=NUM_RUNS, number=1)
    if hasattr(script, '__name__'):
        print(script.__name__)
    else:
        print("Unknown")
    if loops > 1:
        mult = 1e9 / loops
        unitname = "nanoseconds per loop"
    else:
        mult = 1e6
        unitname = "microseconds per run"
    print(" best %s: %d" % (unitname, mult * min(times)))
    times = sorted(times)[:len(times)//2 + 1]
    print(" average %s (excl. outliers): %.2f" % (unitname, mult * sum(times) / len(times)))

########################################################################

# for i in range(30000):
#     print i, crappy_sqrt(i), i ** 0.5, int(i ** 0.5)
#     assert crappy_sqrt(i) == int(i ** 0.5 + 0.5)

run_benchmark(benchmark_while_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_continue_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_addition, MICRO_LOOPCOUNT)
run_benchmark(benchmark_call_script, MICRO_LOOPCOUNT)
run_benchmark(benchmark_call_multiarg_script, MICRO_LOOPCOUNT)
run_benchmark(benchmark_recursive_fibonacci, 1)
run_benchmark(benchmark_fixedmul, 1)
run_benchmark(benchmark_string_iter, 1)
run_benchmark(benchmark_crappy_sqrt, 1)
