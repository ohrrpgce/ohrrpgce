#!/usr/bin/env sq
//
// This is a port of a few of the microbenchmarks in benchmark.hss to Squirrel for comparison


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
    i <- MICRO_LOOPCOUNT
    while (i)
        i -= 1
}

/******************************/

function empty_multiarg_script(a, b, c, d) {
}

function benchmark_call_multiarg_script() {
    end <- MICRO_LOOPCOUNT / 10 + 1
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
    total <- 0
    for (local i = -100000; i < 100000; i += 8000)
        for (local j = -100000; j < 100000; j += 8000)
            total += FixedMul(i, j)
    return total
}

//printnl("fixedmul " + benchmark_fixedmul())   //FIXME: returns wrong result


/******************************/

// FIXME: loops is ignored

function run_benchmark(script, loops) {
    local times = []
    for (local i = 0; i < loops; i++) {
        local timing = clock()
        script()
        times.append(clock() - timing)
    }
    printnl(script.getinfos().name)
    printnl(" Best microseconds per run: " + min(times) * 1e6)
    times = times.sort().slice(0, times.len()/2 + 1)
    //printnl(showarray(times))
    printnl(" average microseconds (excl. outliers): " + 1e6 * sum(times) / times.len())
}

run_benchmark(benchmark_while_loop, MICRO_LOOPCOUNT)
run_benchmark(benchmark_call_multiarg_script, MICRO_LOOPCOUNT)
run_benchmark(benchmark_recursive_fibonacci, 1)
run_benchmark(benchmark_fixedmul, 1)
