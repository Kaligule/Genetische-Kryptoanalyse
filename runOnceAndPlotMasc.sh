#!/bin/bash

# output$1.txt generieren
time ./GenMasc > solvingLogs/output$1.txt

# plotten und pdf aufrauemen
gnuplot -e "dataName ='solvingLogs/output$1.txt'; pdfName='solvingPlots/output$1.pdf'" plotte_output.plt


# ausgabe machen
filelength=$(wc -l < solvingLogs/output$1.txt)
echo "Final Generation: $(($filelength / 1 - 6))"
tail -6 solvingLogs/output$1.txt

#aufraeumen
#rm solvingLogs/output$1.txt
