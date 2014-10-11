#!/bin/bash
anzahlDurchlaufe="$1"

ghc --make -O2 -outputdir=ghc_outputdir GenPerm.hs
echo "compiled"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlotPerm.sh
./plotMultipleOutputs.sh
./plotEliteOutput.sh
rm GenPerm
