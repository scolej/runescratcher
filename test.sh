set -eu

ulimit -m 100
ulimit -t 10

export GUILE_LOAD_PATH=$(pwd)

# Compile everything without optimization for better backtraces?
# find test runes util -name \*scm -exec guild compile -O0 '{}' \;

export GUILE_AUTO_COMPILE=1

# guild compile -O0 test/*

guile='guile --debug'
# $guile runes/scratch.scm 2>&1 #| fold -s -w 80
$guile test/run.scm 2>&1 #| fold -s -w 80
