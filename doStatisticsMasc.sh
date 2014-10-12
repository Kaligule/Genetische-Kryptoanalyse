#!/bin/bash

# Check for proper number of command line args.
EXPECTED_ARGS=1
E_BADARGS=65
if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: ./`basename $0` numberOfTriesToMake"
  exit $E_BADARGS
fi

anzahlDurchlaufe="$1"

ghc --make -O2 -outputdir=ghc_outputdir GenMasc.hs
echo "compiled GenMasc.hs"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlotMasc.sh


./plotMultipleOutputs.sh
echo "plotted Multiplot"

ghc --make -O2 -outputdir=ghc_outputdir convertforEliteplot.hs
ghc --make -O2 -outputdir=ghc_outputdir convertForMonogramAnalysis.hs
for i in `seq 1 $anzahlDurchlaufe`;
do
	./plotEliteOutput.sh "$i"
    echo "Eliteplot $i plotted"
    ./plotMonogramAnalysis.sh "$i"
    echo "MonogramAnalysis $i plotted"
done
rm convertForMonogramAnalysis
rm convertforEliteplot


rm GenMasc
