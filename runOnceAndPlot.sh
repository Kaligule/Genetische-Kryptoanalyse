#!/bin/bash

# output$1.txt generieren
time ./GenSort > output$1.txt

# plotten und pdf aufrauemen
gnuplot -e "dataName ='output$1.txt'; pdfName='solvingPlots/output$1.pdf'" plotte_output.plt
# cp output.pdf ./solvingPlots/output$1.pdf

# ausgabe machen
filelength=$(wc -l < output$1.txt)
echo "Final Generation: $(($filelength - 6))"
tail -5 output$1.txt

#aufraeumen
rm output$1.txt
