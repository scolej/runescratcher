ulimit -m 100
ulimit -t 5
export GUILE_LOAD_PATH=.:/home/sjc/ev/prog/info-bar
guild compile main.scm
guild compile world.scm
guile main.scm 
