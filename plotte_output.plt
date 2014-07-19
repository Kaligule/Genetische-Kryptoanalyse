set termina pdf enhanced color font 'Helvetica,10'
#set yrange [0:360]
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output pdfName 
set key bottom box
plot dataName using 1:2 with lines lc rgb 'red' lw 4 title 'best value',\
     dataName using 1:4 with lines lc rgb 'dark-grey' title 'median value',\
     dataName using 1:6 with lines lc rgb 'grey' title 'worst value'