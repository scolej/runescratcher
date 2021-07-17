#!/bin/sh
set -eu
ulimit -m 100
ulimit -t 30
export GUILE_LOAD_PATH=$(pwd)
export COLUMNS=100 # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46009
guile main.scm
