touch solvingLogs/outputElite.txt
ghc --make -outputdir=ghc_outputdir convertforEliteplot.hs
./convertforEliteplot
rm convertforEliteplot
echo "converted"
gnuplot -e "dataName ='solvingLogs/outputElite.txt'; pdfName='solvingPlots/outputElite.pdf'" plotEliteOutput.plt
echo "ploted"