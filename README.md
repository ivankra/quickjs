# QuickJS-AOT

Futamura projection enabled QuickJS fork - an ahead-of-time JavaScript to native code compiler (proof of concept).

Builds up on the earlier work of adding [tail-call dispatch](https://github.com/ivankra/quickjs/tree/tail) to QuickJS, from which it is relatively easy to implement first Futamura projection (partial specialization of interpreter for a given fixed bytecode input).

`qjsc` gets a new flag `-A` to make it compile each bytecode method to C by unrolling interpreter's loop, stiching together fragments of its C code.  The code is compiled by `clang -O2` with bytecode made available to the compiler as a static const array for constant folding, allowing it to tailor ops to constants in the bytecode.

Basic usage:
```sh
$ ./qjsc -A prog.js && ./a.out
```

## Results

It works, but performance improvement is relatively modest (Mac M4, Debian 13 VM, median of 20 runs, higher is better - score inversely proportional to runtime):

| Benchmark         | [base](https://github.com/ivankra/quickjs/commit/b22685617750e51d95f643d8eb1382dd09dd3bf1) | % | [tail](https://github.com/ivankra/quickjs/commits/tail-20251204/) | % | qjsc -A |
|-------------------|----------|-------------|----------|-------------|----------|
| Richards          | 1776     | +10.33%     | 1960     | +25.00%     | 2450     |
| DeltaBlue         | 1850     | -4.11%      | 1774     | +24.65%     | 2212     |
| Crypto            | 2018     | +19.90%     | 2419     | +77.93%     | 4305     |
| RayTrace          | 3413     | +5.55%      | 3602     | +13.95%     | 4105     |
| EarleyBoyer       | 4093     | -1.47%      | 4033     | +14.11%     | 4602     |
| RegExp            | 1012     | +1.04%      | 1022     | +3.28%      | 1056     |
| Splay             | 5685     | +1.88%      | 5792     | +9.53%      | 6344     |
| NavierStokes      | 3710     | +40.97%     | 5230     | +51.93%     | 7946     |
| **Geomean v8-v7** | **2582** | **+8.45%**  | **2800** | **+25.64%** | **3518** |
| SplayLatency      | 19718    | +0.38%      | 19794    | +9.70%      | 21714    |
| PdfJS             | 8481     | +11.26%     | 9436     | +19.55%     | 11281    |
| Mandreel          | 1957     | +20.82%     | 2364     | +68.05%     | 3973     |
| MandreelLatency   | 16673    | +27.78%     | 21304    | +57.05%     | 33458    |
| Gameboy           | 14845    | +27.57%     | 18938    | +46.12%     | 27672    |
| CodeLoad          | 33894    | -1.03%      | 33547    | +0.54%      | 33729    |
| Box2D             | 7189     | +17.28%     | 8431     | +40.78%     | 11869    |
| zlib              | 3200     | +53.55%     | 4914     | +1.27%      | 4977     |
| Typescript        | 24431    | +11.23%     | 27175    | +23.39%     | 33532    |
| **Geomean v8-v9** | **5425** | **+13.29%** | **6146** | **+26.65%** | **7784** |

Another implemented mode `qjsc -B` produces less optimizable code with a jump label at every opcode - essentially inhibiting C compiler from optimizing across opcodes. Can be useful as approximation for a possible baseline/template JIT. It shows only about half of the performance improvement vs `qjsc -A` on average.

## Related work

* [QuickJIT](https://github.com/bnoordhuis/quickjit): unfinished project which took a much more labor-intensive approach of converting each opcode implementation by hand and thus remains fairly incomplete. Unlike it, here we systematically extract opcode implementations by parsing preprocessed interpreter code (made easy after tail-call dispatch change), this allowed us to flesh out a fully working prototype in couple of days.
* [LuaAOT](https://github.com/hugomg/lua-aot-5.4) ([paper](https://www.inf.puc-rio.br/~roberto/docs/paper-aot-preprint.pdf)): similar project for Lua 5.3/5.4.
* [weval](https://github.com/bytecodealliance/weval) ([blog](https://cfallin.org/blog/2024/08/28/weval/), [paper](https://arxiv.org/html/2411.10559v1))

## Future ideas

A basic JIT could be implemented by compiling new hot bytecode functions to `.so` and dynamically loading it with `dlopen()`. Linking with `quickjs.c` would be a challenge due to its heavy use of static functions.

## Profiling recipes

Coverage:
```sh
./qjsc -A -e -o a.c navier-stokes.js &&
clang -O2 -g -fprofile-instr-generate -fcoverage-mapping -o a.out a.c libquickjsaot.a -lm &&
LLVM_PROFILE_FILE=a.profraw ./a.out &&
llvm-profdata merge -o a.profdata a.profraw &&
llvm-cov show ./a.out -instr-profile=a.profdata | less
```

Callgraph profile with google-perftools:
```sh
sudo apt install google-perftools libgoogle-perftools-dev graphviz
./qjsc -A -e -o a.c navier-stokes.js &&
clang -O2 -g -fno-omit-frame-pointer -o a.out a.c libquickjsaot.a -lm -lprofiler &&
CPUPROFILE=a.prof CPUPROFILE_FREQUENCY=1000 ./a.out &&
google-pprof --web a.out a.prof
```

Fine-grained CPU profile:
```sh
sudo sysctl kernel.perf_event_paranoid=1
./qjsc -A -e -o a.c navier-stokes.js &&
clang -O2 -g -fno-omit-frame-pointer -o a.out a.c libquickjsaot.a -lm &&
perf record -F 10000 --call-graph dwarf ./a.out &&
perf annotate --tui
```
