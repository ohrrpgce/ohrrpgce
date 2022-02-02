//#!/usr/bin/env sq
//
// This is a port of a few of the microbenchmarks in benchmark.hss to Squirrel/Quirrel for comparison


NUM_RUNS <- 100
MICRO_LOOPCOUNT <- 1000


/****** Helper functions ******/

function sum(arr) {
    local ret = 0
    foreach (x in arr)
        ret += x
    return ret
}

function min(arr) {
    local ret = 1.79e308
    foreach (x in arr)
        if (x < ret)
            ret = x
    return ret
}

function printnl(str)
    print(str + "\n")

function showarray(arr) {
    local ret = "["
    foreach (i, x in arr) {
        if (i)
            ret += ", "
        ret += "" + x //.tostring()
    }
    return ret + "]"
}

/******************************/

function benchmark_while_loop() {
    local i = MICRO_LOOPCOUNT
    while (i)
        i -= 1
}

function benchmark_continue_loop() {
    local i = MICRO_LOOPCOUNT
    while (true) {
        i -= 1
        if (i)
            continue
        break
    }
}

function benchmark_addition() {
    local x = 0, y = 0
    for (local i = 0; i < MICRO_LOOPCOUNT / 10; i++) {
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
    }
}

/******************************/

function empty_script() {
}

function empty_multiarg_script(a, b, c, d) {
}

function benchmark_call_script() {
    for (local i = 0; i < MICRO_LOOPCOUNT / 10; i++) {
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
    }
}

function benchmark_call_multiarg_script() {
    local end = MICRO_LOOPCOUNT / 10 + 1
    for (local i = 0; i < end; i++) {
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
    }
}

/******************************/

function fibonacci(n) {
    if (n < 2) return 1
    return fibonacci(n-2) + fibonacci(n-1)
}

function benchmark_recursive_fibonacci() {
    fibonacci(14)
}

/******************************/

function FixedMul(a, b) {
    if (a >= 0)
        return (a / 0x10000) * b + ((a & 0xffff) * b + 0x8000) / 0x10000
    else
        // fixme: not sure the rounding here is correct
        return (a / 0x10000) * b - ((-a & 0xffff) * b + 0x8000) / 0x10000
}

function benchmark_fixedmul() {
    local total = 0
    for (local i = -100000; i < 100000; i += 8000)
        for (local j = -100000; j < 100000; j += 8000)
            total += FixedMul(i, j)
    return total
}

//printnl("fixedmul " + benchmark_fixedmul())   //FIXME: returns wrong result

/******************************/

function benchmark_string_iter() {
    for (local i = 0; i < 8; i++) {
        local s = "The quick onyx goblin jumps over the lazy dwarf. Bright vixens jump, lazy fowl quack. Amazingly few discotheques provide jukeboxes."
        local words = 1
        local hsh = 0
        foreach (ch in s) {
            if (ch == 32) words += 1
            hsh += words * ch
        }
        //if (i==0) printnl("HASH " + hsh)
    }
}

/******************************/

function crappy_sqrt(fi) {
  local start, divi, approx = -1
  if (fi >= 32581)
      return 181
  else {

        if (fi < 100 ) {
            start = 0
            divi = start * start
        } else {
            if (fi >= 22500)
                start = 150
            else {
                if (fi >= 14400)
                    start = 120
                else {
                    if (fi >= 8100)
                        start = 90
                    else {
                        if (fi >= 4225)
                            start = 65
                        else {
                            if (fi >= 1600)
                                start = 40
                            else {
                                if (fi >= 900)
                                    start = 30
                                else {
                                    if (fi >= 400)
                                        start = 20
                                    else
                                        start = 10
                                }
                            }
                        }
                    }
                }
            }
            divi = start * start
            if (fi / 3 > divi / 2) {
                start = (start / 5) * 6
                divi = start * start
            }
            if (fi / 4 > divi / 3) {
                start = (start / 7) * 8
                divi = start * start
            }
        }
        while (approx == -1) {
            if (divi >= fi) {
                approx = start
            } else {
                start += 1
                divi = start * start
            }
        }
        if (divi == fi) {
            return approx
        } else {
            if ((divi - approx) == fi)
                return approx - 1
            else
                return fi / approx + 1
        }
    }
}

// Test flow control
function benchmark_crappy_sqrt() {
    for (local i = 0; i < 81; i++)
        crappy_sqrt(i)
}


/******************************/

function run_benchmark(script, loops) {
    local times = []
    for (local i = 0; i < NUM_RUNS; i++) {
        local timing = clock()
        script()
        times.append(clock() - timing)
    }

    if (_version_.contains("Quirrel"))
        // Quirrel
        printnl(script.getfuncinfos().name)
    else
        // Squirrel
        printnl(script.getinfos().name)
    local mult, unitname
    if (loops > 1) {
        mult = 1e9 / loops
        unitname = "nanoseconds per loop"
    } else {
        mult = 1e6
        unitname = "microseconds per run"
    }
    printnl(" best " + unitname + ": " + mult * min(times))
    times = times.sort().slice(0, times.len()/2 + 1)
    //printnl(showarray(times))
    printnl(" average " + unitname + " (excl. outliers): " + mult * sum(times) / times.len())
}

run_benchmark(benchmark_while_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_continue_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_addition, MICRO_LOOPCOUNT)
run_benchmark(benchmark_call_script, MICRO_LOOPCOUNT)
run_benchmark(benchmark_call_multiarg_script, MICRO_LOOPCOUNT)
run_benchmark(benchmark_recursive_fibonacci, 1)
run_benchmark(benchmark_fixedmul, 1)
run_benchmark(benchmark_string_iter, 1)
run_benchmark(benchmark_crappy_sqrt, 1)
