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
echo "Dollar 1 ist: $1"
echo "anzahlDurchlaufe ist $anzahlDurchlaufe"
ghc --make -O2 -outputdir=ghc_outputdir GenMasc.hs
echo "compiled GenMasc.hs"
seq $anzahlDurchlaufe | parallel --gnu ./runOnceAndPlotMasc.sh


./plotMultipleOutputs.sh
echo "plotted Multiplot"

ghc --make -O2 -outputdir=ghc_outputdir convertforEliteplot.hs
for i in `seq 1 $anzahlDurchlaufe`;
do
	nameStock="output"$i"Elite"
	touch solvingLogs/$nameStock.txt
	./convertforEliteplot "$i"
	gnuplot -e "dataName ='solvingLogs/$nameStock.txt'; pdfName='solvingPlots/$nameStock.pdf'" plotEliteOutput.plt
    echo "Eliteplot $i plotted"
done
rm convertforEliteplot

# moving things to ghc_outputdir seems to break things
ghc --make -O2 convertforMonogramAnalysis.hs
echo "anzahlDurchlaufe ist anzahlDurchlaufe"
for i in `seq 1 $anzahlDurchlaufe`;
do
	nameStock="output"$i"MonogramAnalysis"
	touch solvingLogs/$nameStock.txt
	./convertforMonogramAnalysis "$i"
	# runhaskell convertforMonogramAnalysis.hs "$i"
	gnuplot -e "dataName ='solvingLogs/$nameStock.txt'; pdfName='solvingPlots/$nameStock.pdf'" plotMonogramAnalysis.plt
    echo "MonogramAnalysis $i plotted"
done
rm convertforMonogramAnalysis convertforMonogramAnalysis.hi convertforMonogramAnalysis.o

rm GenMasc
