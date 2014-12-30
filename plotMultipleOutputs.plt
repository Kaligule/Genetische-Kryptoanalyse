set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output pdfName 
set key bottom right box

# line styles for ColorBrewer Dark2
# for use with qualitative/categorical data
# provides 8 dark colors based on Set2
# compatible with gnuplot >=4.2
# author: Anna Schneider
# line styles
set style line 1 lc rgb '#1B9E77' # dark teal
set style line 2 lc rgb '#D95F02' # dark orange
set style line 3 lc rgb '#7570B3' # dark lilac
set style line 4 lc rgb '#E7298A' # dark magenta
set style line 5 lc rgb '#66A61E' # dark lime green
set style line 6 lc rgb '#E6AB02' # dark banana
set style line 7 lc rgb '#A6761D' # dark tan
set style line 8 lc rgb '#666666' # dark gray
set style line 9 lc rgb 'dark-blue' # blue



obergrenze=system("head -1 " . dataName . "| cut -c1-2 --complement") + 0

# plot for [col=2:11] dataName using 1:col with lines title columnheader, obergrenze linecolor rgb "black" title "Fittnes to reach"
plot for [col=2:11] dataName using 1:col with lines ls (col-1) lw 5 title columnheader, obergrenze  linecolor rgb "black" title "Fittnes to reach"
