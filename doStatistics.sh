#!/bin/bash
anzahlDurchlaufe="$1"

rm GenSort
ghc --make -outputdir=ghc_outputdir GenSort.hs
echo "compiled"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlot.sh