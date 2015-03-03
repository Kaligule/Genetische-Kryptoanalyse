set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output pdfName 
set key opaque
set key left top box

# line styles for ColorBrewer Dark2
# for use with qualitative/categorical data
# provides 8 dark colors based on Set2
# compatible with gnuplot >=4.2
# author: Anna Schneider
# line styles
set style line 1 lc rgb 'red' # dark teal
set style line 2 lc rgb 'blue' # dark orange
set style line 3 lc rgb 'olive' # dark lilac
set style line 4 lc rgb 'navy' # dark magenta
set style line 5 lc rgb 'brown' # dark lime green
set style line 6 lc rgb 'dark-violet' # dark banana
set style line 7 lc rgb '#A6761D' # dark tan
set style line 8 lc rgb '#666666' # dark gray
set style line 9 lc rgb 'dark-blue' # blue




plot for [col=2:7] dataName using 1:col with lines ls (col-1) lw 3 title columnheader
