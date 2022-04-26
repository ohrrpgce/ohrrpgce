#!/usr/bin/env python3
#
# This is a port of a few of the microbenchmarks in benchmark.hss to Python for comparison
#
try:
    import timeit
except:
    # For MicroPython
    import time

    class timeit:
        @classmethod
        def repeat(cls, script, repeat, number=1, setup=None):
            ret = []
            for i in range(repeat):
                if setup:
                    setup()
                timing = time.time()
                script()
                timing = time.time() - timing
                ret.append(timing)
            return ret


NUM_RUNS = 100
MICRO_LOOPCOUNT = 1000

########################################################################

def benchmark_for_loop():
    for i in range(MICRO_LOOPCOUNT):
        pass

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

def benchmark_increment():
  x, y = 0, 0
  for i in range(MICRO_LOOPCOUNT // 10 + 1):
      x += y
      x += y
      x += y
      x += y
      x += y
      x += y
      x += y
      x += y
      x += y
      x += y

def benchmark_array_create_delete():
    for i in range(MICRO_LOOPCOUNT):
        arr = [0] * 8
        arr = None

def benchmark_array_length():
    for i in range(MICRO_LOOPCOUNT // 10 + 1):
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)
        len(testarray)

testarray = [0] * MICRO_LOOPCOUNT

def benchmark_array_index():
    for i in range(MICRO_LOOPCOUNT // 10 + 1):
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]
        testarray[i]

def benchmark_array_foreach():
    for val in testarray:
        val

def benchmark_array_sum():
    total = 0
    for val in testarray:
        total += val

def benchmark_array_append():
    arr = []
    for i in range(100):
        arr.append(i)

def benchmark_string_append():
    var = ""
    for i in range(MICRO_LOOPCOUNT // 10 + 1):
        var += "a"
        var += "a"
        var += "a"
        var += "a"
        var += "a"
        var += "a"
        var += "a"
        var += "a"
        var += "a"
        var += "a"

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

bubbles = None

def benchmark_bubble_fill():
    global bubbles
    bubbles = [0] * 40
    for i in range(len(bubbles)):
        bubbles[i] = (24461 * i) % 32767

def benchmark_bubble_sort():
    global bubbles
    for i in range(1, len(bubbles)):
        for j in range(0, i):
            if bubbles[j] > bubbles[i]:
                bubbles[j], bubbles[i] = bubbles[i], bubbles[j]
    # for i in range(0, len(bubbles) - 1):
    #     assert bubbles[i] <= bubbles[i+1]
    # print(sum(bubbles))

########################################################################

def run_benchmark(script, loops, scoremult = 1, init_func = ""):
    times = timeit.repeat(script, repeat=NUM_RUNS, number=1, setup=init_func)
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
    displayval = mult * sum(times) / len(times)
    print(" average %s (excl. outliers): %.2f" % (unitname, displayval))
    global score
    score += int(displayval * scoremult)

########################################################################

# for i in range(30000):
#     print i, crappy_sqrt(i), i ** 0.5, int(i ** 0.5)
#     assert crappy_sqrt(i) == int(i ** 0.5 + 0.5)

score = 0
run_benchmark(benchmark_for_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_while_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_continue_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_addition, MICRO_LOOPCOUNT)
run_benchmark(benchmark_increment, MICRO_LOOPCOUNT)
run_benchmark(benchmark_array_create_delete, MICRO_LOOPCOUNT, 0.2)
run_benchmark(benchmark_array_length, MICRO_LOOPCOUNT)
run_benchmark(benchmark_array_index, MICRO_LOOPCOUNT)
run_benchmark(benchmark_array_foreach, MICRO_LOOPCOUNT, 0)  # mult=0 because HS lacks it
run_benchmark(benchmark_array_sum, MICRO_LOOPCOUNT)
run_benchmark(benchmark_array_append, 100)
run_benchmark(benchmark_string_append, MICRO_LOOPCOUNT, 0.25)
run_benchmark(benchmark_call_script, MICRO_LOOPCOUNT)
run_benchmark(benchmark_call_multiarg_script, MICRO_LOOPCOUNT)
print("\nGeneral benchmarks\n")
run_benchmark(benchmark_recursive_fibonacci, 1, 2)
run_benchmark(benchmark_fixedmul, 1)
run_benchmark(benchmark_string_iter, 1)
run_benchmark(benchmark_crappy_sqrt, 1)
run_benchmark(benchmark_bubble_fill, 40)
run_benchmark(benchmark_bubble_sort, 1, init_func=benchmark_bubble_fill)
print("Total time score: %d" % score)
