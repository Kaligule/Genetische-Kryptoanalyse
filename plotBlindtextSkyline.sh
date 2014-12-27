runhaskell convertforBlindTextMonogramAnalysis.hs > BlindtextSkyline.txt
gnuplot -e "dataName='BlindtextSkyline.txt'; pdfName='Lyx/Pictures/Generated/BlindtextSkyline.pdf'" plotBlindtextSkyline.plt
rm BlindtextSkyline.txt
