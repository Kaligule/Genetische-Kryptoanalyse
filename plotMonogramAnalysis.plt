set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'monogramms'
set ylabel 'frequency'
set output pdfName 
set key right top box

set style line 1 lt rgb "blue" lw 3 pt 5
set style line 2 lt rgb "red" lw 3 pt 5

plot dataName using 3:xticlabels(1) with boxes fs solid 1 ls 2 title columnheader,\
     dataName using 2:xticlabels(1) with histeps ls 1 title columnheader