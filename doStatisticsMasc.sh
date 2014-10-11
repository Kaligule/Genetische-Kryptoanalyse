#!/bin/bash
anzahlDurchlaufe="$1"

ghc --make -O2 -outputdir=ghc_outputdir GenMasc.hs
echo "compiled GenMasc.hs"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlotMasc.sh
./plotMultipleOutputs.sh
echo "plottet Multiplot"
./plotEliteOutput.sh
echo "plottet Eliteplot"
rm GenMasc
