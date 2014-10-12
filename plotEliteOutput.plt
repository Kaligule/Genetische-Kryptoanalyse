set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output pdfName 
set key bottom box

plot for [col=2:5] dataName using 1:col with lines title columnheader