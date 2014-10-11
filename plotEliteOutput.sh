touch solvingLogs/outputElite.txt
ghc --make -outputdir=ghc_outputdir convertforEliteplot.hs
./convertforEliteplot
rm convertforEliteplot
gnuplot -e "dataName ='solvingLogs/output1Elite.txt'; pdfName='solvingPlots/output1Elite.pdf'" plotEliteOutput.plt
