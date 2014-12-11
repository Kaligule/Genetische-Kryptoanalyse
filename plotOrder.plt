set termina pdf enhanced color font 'Helvetica,10'

set xrange [-0.5:10.5]
set xtics 1
set xlabel ''
set yrange [0:12]
set ylabel ''

unset key;
unset tics;
unset border

set output pdfName 

set linestyle 1 linecolor rgb 'black' linewidth 3 pointtype 5

plot dataName using 0:1 with boxes ls 1 title "Reihenfolge"
