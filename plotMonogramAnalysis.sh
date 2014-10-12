# Check for proper number of command line args.
EXPECTED_ARGS=1
E_BADARGS=65
if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: ./`basename $0` numberOfLogFileToAnalyse"
  exit $E_BADARGS
fi

number=$1
nameStock="output"$number"MonogramAnalysis"
echo "number is $number and nameStock is $nameStock"
touch solvingLogs/$nameStock.txt
./convertForMonogramAnalysis "$number"
echo "try plotting solvingLogs/$nameStock.txt to solvingPlots/$nameStock.pdf"
gnuplot -e "dataName ='solvingLogs/$nameStock.txt'; pdfName='solvingPlots/$nameStock.pdf'" plotMonogramAnalysis.plt
