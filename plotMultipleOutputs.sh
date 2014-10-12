touch solvingLogs/outputCombination.txt
ghc --make -outputdir=ghc_outputdir convertforMultiplot.hs
./convertforMultiplot
rm convertforMultiplot
gnuplot -e "dataName ='solvingLogs/outputCombination.txt'; pdfName='solvingPlots/outputCombination.pdf'" plotMultipleOutputs.plt
