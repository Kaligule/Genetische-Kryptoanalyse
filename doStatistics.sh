#!/bin/bash
ghc --make GenSort.hs
for (( i=1; i <= $1; i++ ))
do
	time ./GenSort > output.txt
	gnuplot -p plotte_output.plt
	filelength=$(wc -l < output.txt)
	echo "Final Generation: $(($filelength - 6))"
	tail -5 output.txt
	cp output.pdf ./solvingPlots/output$i.pdf
done