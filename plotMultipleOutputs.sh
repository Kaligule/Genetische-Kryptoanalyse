touch outputCombination.txt
ghc convertforMultiplot.hs
./convertforMultiplot
rm convertforMultiplot
gnuplot -e "dataName ='outputCombination.txt'; pdfName='solvingPlots/outputCombination.pdf'" multiplot_output.plt
