#!/bin/bash
ghc --make GenSort.hs
time ./GenSort > output.txt
gnuplot -p plotte_output.plt
filelength=$(wc -l < output.txt)
echo "Final Generation: $(($filelength - 6))"
tail -5 output.txt