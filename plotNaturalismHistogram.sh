runhaskell NaturalismHistogram.hs > NaturalismHistogram.txt
gnuplot -e "dataName ='NaturalismHistogram.txt'; pdfName='Lyx/Pictures/Generated/NaturalismHistogram.pdf'" plotNaturalismHistogram.plt
#cat NaturalismHistogram.txt
rm NaturalismHistogram.txt


