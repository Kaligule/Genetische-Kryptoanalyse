set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output 'output.pdf'
set key bottom box
plot 'output.txt' using 1:2 with lines lc rgb 'red' lw 4 title 'best value',\
     'output.txt' using 1:3 with lines lc rgb 'dark-grey' title 'median value',\
     'output.txt' using 1:4 with lines lc rgb 'grey' title 'worst value'