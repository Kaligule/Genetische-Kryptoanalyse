# Check for proper number of command line args.
EXPECTED_ARGS=1
E_BADARGS=65
if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: ./`basename $0` otputNumberToPlot"
  exit $E_BADARGS
fi
number="$1"
nameStock="output"$number"Elite"
touch solvingLogs/$nameStock.txt
./convertForMonogramAnalysis "$number"
gnuplot -e "dataName ='solvingLogs/$nameStock.txt'; pdfName='solvingPlots/$nameStock.pdf'" plotEliteOutput.plt
