
function printArray {
    for item in ${array[*]}
    do
	printf "%s\n" $item
    done
}





# noManipulation
nameStock=$"noManipulation"
array=(1 2 3 4 5 6 7 8 9 10 11)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# swapMutate
nameStock=$"swapMutate"
array=(1 2 3 10 5 6 7 8 9 4 11)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# listSwapMutate
nameStock=$"listSwapMutate"
array=(1 2 9 10 6 7 8 3 4 5 11) 
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# revMutate
nameStock=$"revMutate"
array=(1 2 3 4 8 7 6 5 9 10 11)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# blockSwapMutate
nameStock=$"blockSwapMutate"
array=(9 10 11 1 2 3 4 5 6 7 8)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# shuffelMutate
nameStock=$"shuffelMutate"
array=(1 2 3 6 8 5 7 4 9 10 11)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# shiftMutate
nameStock=$"shiftMutate"
array=(1 2 4 5 6 3 7 8 9 10 11)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# completeShiftMutate
nameStock=$"completeShiftMutate"
array=(11 1 2 3 4 5 6 7 8 9 10)
printArray $array > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt




rm tmp.txt
