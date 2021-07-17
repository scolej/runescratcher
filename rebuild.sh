export GUILE_LOAD_PATH=$(pwd)
find -type f -iname \*.scm -exec guild compile {} \;
