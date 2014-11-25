# Nomanipulation
nameStock=$"noManipulation"
result=$"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# swapMutate
nameStock=$"swapMutate"
result=$"1\n2\n3\n10\n5\n6\n7\n8\n9\n4\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# listSwapMutate
nameStock=$"listSwapMutate"
result=$"1\n2\n9\n10\n6\n7\n8\n3\n4\n5\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# revMutate
nameStock=$"revMutate"
result=$"1\n2\n3\n4\n8\n7\n6\n5\n9\n10\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# blockSwapMutate
nameStock=$"blockSwapMutate"
result=$"\n9\n10\n11\n1\n2\n3\n4\n5\n6\n7\n8"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# shuffelMutate
nameStock=$"shuffelMutate"
result=$"1\n2\n3\n6\n8\n5\n7\n4\n9\n10\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt




rm tmp.txt
