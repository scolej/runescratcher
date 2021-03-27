set -exu

export GUILE_LOAD_PATH=$(pwd)
export GUILE_AUTO_COMPILE=fresh

# guild compile -O0 test.scm
# guild compile -O0 world.scm
# guild compile -O0 game.scm

# guile --debug -l world-test.scm -c '((@ (world-test) run-all))'
guile --debug world-test.scm
