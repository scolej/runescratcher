ulimit -m 100
ulimit -t 30
export GUILE_LOAD_PATH=$(pwd)
guile main.scm
