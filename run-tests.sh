#!/bin/bash
BEMU=./bemu
OPTIONS=""
NONDET=0        # Set for tests that are nondeterministic
FAIL=

run_one() {
    file="$1"

    bt_run=$($BEMU -o "$OPTIONS" -d  "tests/$file.bin" 2>&1)
    err="$?"
    if [ -z "$FAIL" -a "$err" -ne 0 ]; then
        echo "[$file] FAIL: return code $err" >&2
        echo "$bt_run";
        exit 1;
    fi 
    em_run=$($BEMU -o "$OPTIONS" -de "tests/$file.bin" 2>&1)

    shift
    while [ $# -gt 0 ]; do
        expect="$1"
        shift
        if ! echo "$bt_run" | grep -qF "$expect"; then
            echo "[$file] FAIL: Expecting: '$expect'" >&2
            echo "$bt_run";
            exit 1;
        fi
    done

    if [ -z "$NONDET" -a "$bt_run" != "$em_run" ]; then
        echo "[$file] FAIL: BT and emulation mismatch" >&2
        echo "[BT result]"
        echo "$bt_run"
        echo "[Emulation result]"
        echo "$em_run"
        exit 1;
    fi

    echo "[$file] OK"
}

run_one sancheck "[00] 800003b8 [01] 00000718 [02] 00000718 [03] 00000718"
OPTIONS=clock
run_one litmus   'All tests PASSED!'
OPTIONS=
run_one bench1   "[00] 00000001 [01] 00000367 [02] 00000001 [03] 00000367"
run_one bench2   "[80000048] Done" "[00] 00000001 [01] 00000000 [02] 00000000 [03] 00000000"
run_one bench3   "[80000c38] Done" "[28] 80000c3c [29] 00000000 [30] 00000000 [31] 00000000"
run_one bench4   "[00] 991727a0 [01] 5096a491 [02] 00000000 [03] 00000000"
run_one supervisor "[00] ffffabcd"
run_one align    "[00] ffffabcd"
run_one qsort    "[00] 00001111"
run_one jmptab   "[00] 00000007" "Hello."
NONDET=1
FAIL=1
run_one trap     "Illegal memory reference" "00002000"
run_one trap2    "Illegal memory reference" "00002000"

ulimit -t 2
OPTIONS="clock"
NONDET=
FAIL=
run_one timer    "[00] ffffabcd"
