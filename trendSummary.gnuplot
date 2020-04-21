set terminal svg
set style data linespoints
set title "wombat"
set key autotitle columnhead
plot 'temp.trend' using 1:15
