set termina pdf enhanced color font 'Helvetica,10'
set ylabel 'naturalism'
set xlabel 'Permutations-Schlüssel'
set output pdfName 
set nokey
set xtics rotate by -60 nomirror scale 0.5
set yrange [0:*]
set ytics nomirror scale 0.1
set datafile separator ";"
set border 0
set boxwidth 0.8 
set tics out

set style line 1 lt rgb "red" lw 3

plot dataName using 2:xticlabels(1) ls 1 with boxes notitle