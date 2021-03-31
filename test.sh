set -exu

ulimit -m 100
ulimit -t 5

export GUILE_LOAD_PATH=$(pwd) GUILE_AUTO_COMPILE=1

# for f in *.scm; do
#     guild compile -O0 "$f"
# done

guile='guile --debug'
$guile --debug test-all.scm
