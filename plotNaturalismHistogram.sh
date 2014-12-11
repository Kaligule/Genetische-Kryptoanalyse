runhaskell NaturalismHistogram.hs > NaturalismHistogram.txt
gnuplot -e "dataName ='NaturalismHistogram.txt'; pdfName='solvingPlots/NaturalismHistogram.pdf'" plotNaturalismHistogram.plt
#cat NaturalismHistogram.txt
rm NaturalismHistogram.txt


