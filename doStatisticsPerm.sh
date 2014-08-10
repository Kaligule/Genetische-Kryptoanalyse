#!/bin/bash
anzahlDurchlaufe="$1"

ghc --make -outputdir=ghc_outputdir GenPerm.hs
echo "compiled"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlot.sh
rm GenPerm
