set terminal png size 600,200 font "Sans,8"
set output "plot.png"

set timefmt "%Y-%m"
set datafile separator ";"

set grid
unset key

set ytics out 10
set yrange [0:100]

set xdata time
# month spaced tics
# (/ (* 60 60 24 365) 12.0)
set xtics out 2628000 format "%m" timedate
set xrange ["2021-03":"2022-03"]

# function for duration on a row in hours
dur(a, b) = (strptime("%Y-%m-%d %H:%M", b) - strptime("%Y-%m-%d %H:%M", a)) / 60 / 60

plot "< sed 's/ -- /;/' < timelog.txt" \
     using (strptime("%Y-%m-%d", stringcolumn(1)))\
     :(dur(stringcolumn(1),stringcolumn(2))) \
     smooth cumulative with lines lc "black"
