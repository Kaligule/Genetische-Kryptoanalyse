set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output pdfName 
set key left top box

obergrenze=system("head -1 " . dataName . "| cut -c1-2 --complement") + 0

plot for [col=2:5] dataName using 1:col with lines title columnheader, obergrenze linecolor rgb "black" title "Fittnes to reach"