#!/bin/bash
anzahlDurchlaufe="$1"

ghc --make -outputdir=ghc_outputdir GenMasc.hs
echo "compiled"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlotMasc.sh
./plotMultipleOutputs.sh
rm GenMasc
