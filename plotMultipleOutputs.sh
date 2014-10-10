touch outputCombination.txt
ghc --make -outputdir=ghc_outputdir convertforMultiplot.hs
./convertforMultiplot
rm convertforMultiplot
gnuplot -e "dataName ='outputCombination.txt'; pdfName='solvingPlots/outputCombination.pdf'" multiplot_output.plt
