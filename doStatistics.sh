#!/bin/bash
anzahlDurchlaufe="$1"

ghc --make -outputdir=ghc_outputdir GenSort.hs
echo "compiled"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlot.sh
rm GenSort
