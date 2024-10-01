import subprocess
import os
time = " ; time ./a.out "
main = "./main.native "
link = "runtime.c cinterop.c -o a.out "
progs = ["pset6programs/alexbrian.oat ", "llprograms/matmul.ll ", "pset6programs/bintodec.oat ", "pset6programs/det.oat ", "pset6programs/rec_knapsack.oat "]

for prog in progs: 
    for opt in [" ", "-O1 "]:
        for flags in ["--liveness trivial --regalloc none ", "--liveness dataflow --regalloc greedy ", "--liveness dataflow --regalloc better ", "--clang "]:
            cmd = main + opt + flags + prog + link 
            print(cmd + time)
            os.system(cmd + time)
            # os.system(time)
