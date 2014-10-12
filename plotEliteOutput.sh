# Check for proper number of command line args.
EXPECTED_ARGS=1
E_BADARGS=65
if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: ./`basename $0` otputNumberToPlot"
  exit $E_BADARGS
fi
number="$1"
echo "`basename $0` got its number $1"

dName1="solvingLogs/output"
dName2="$number"
dName3="Elite.txt"
dName=$dName1$dName2$dName3

pName1="solvingPlots/output"
pName2="$number"
pName3="Elite.pdf"
pName=$pName1$pName2$pName3

touch solvingLogs/output$numberElite.txt
ghc --make -outputdir=ghc_outputdir convertforEliteplot.hs
./convertforEliteplot "$1"
rm convertforEliteplot
gnuplot -e "dataName ='$dName'; pdfName='$pName'" plotEliteOutput.plt
