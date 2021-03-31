set -exu

ulimit -m 100
ulimit -t 5

# for f in *.scm; do
#     guild compile -O0 "$f"
# done

export GUILE_LOAD_PATH=$(pwd) GUILE_AUTO_COMPILE=fresh

# guild compile -O0 test/world.scm

guile='guile --debug'
$guile --debug test/all.scm
