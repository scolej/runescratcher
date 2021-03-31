set -eu

ulimit -m 100
ulimit -t 5

export GUILE_LOAD_PATH=$(pwd)

# Compile everything without optimization for better backtraces?
# find test runes util -name \*scm -exec guild compile -O0 '{}' \;

export GUILE_AUTO_COMPILE=1

guile='guile --debug'
$guile --debug test/run.scm
