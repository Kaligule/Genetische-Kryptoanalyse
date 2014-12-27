set termina pdf enhanced color font 'Helvetica,10'
set xlabel 'monogramms'
set ylabel 'frequency'
set output pdfName 
set key right top box

set style line 1 lt rgb "blue" lw 3 pt 5
set style line 2 lt rgb "red" lw 3 pt 5

# catalog of blindtext1
# [('E',161),('A',144),('T',142),('N',131),('I',127),('O',106),('S',94),('H',93),('R',85),('L',68),('D',55),('U',54),('C',54),('M',42),('G',36),('F',32),('P',30),('B',25),('Y',17),('V',16),('W',14),('J',5),('Q',3),('K',3),('Z',1),('X',0)]

plot dataName using 3:xticlabels(1) with boxes fs solid 1 ls 2 title columnheader,\
     dataName using 2:xticlabels(1) with histeps ls 1 title columnheader
