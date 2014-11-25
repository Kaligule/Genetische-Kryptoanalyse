ghc --make pathManipulation.lhs
# chmod +x pa

# Nomanipulation
nameStock=$"noManipulation"

echo "[[(11,[],[1,2,3,4,5,6,7,8,9,10,11],[],[])]]" | ./pathManipulation -o ./Lyx/Pictures/Generated/${nameStock}Graph.svg -h 500

result=$"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# swapMutate
nameStock=$"swapMutate"

echo "[[(11,[4,10],[1,2,3,4,5,6,7,8,9,10,11],[],[])],[(11,[4,10],[1,2,3,10,5,6,7,8,9,4,11],[],[])]]" | ./pathManipulation -o ./Lyx/Pictures/Generated/${nameStock}.svg -w 500

result=$"1\n2\n3\n10\n5\n6\n7\n8\n9\n4\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# listSwapMutate
nameStock=$"listSwapMutate"

echo "[[(11,[],[1,2,3,4,5,6,7,8,9,10,11],[(2,3),(5,6),(8,9),(10,11)],[]), (11, [], [1,2,9,10,6,7,8,3,4,5,11],[],[(2,9),(10,6),(8,3),(5,11)]) ]]" | ./pathManipulation -o ./Lyx/Pictures/Generated/${nameStock}.svg -h 500

result=$"1\n2\n9\n10\n6\n7\n8\n3\n4\n5\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# revMutate
nameStock=$"revMutate"

echo "[[(11,[5,8],[1,2,3,4,5,6,7,8,9,10,11],[],[])],[(11,[5,8],[1,2,3,4,8,7,6,5,9,10,11],[],[])]]" | ./pathManipulation -o ./Lyx/Pictures/Generated/${nameStock}.svg -w 500

result=$"1\n2\n3\n4\n8\n7\n6\n5\n9\n10\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# blockSwapMutate
nameStock=$"blockSwapMutate"

#Das Kreisdiagram funktioniert hier nicht so richtig. Man sollte den anfang und das Ende besser markieren
echo "[[(11,[],[1,2,3,4,5,6,7,8,9,10,11],[(8,9)],[])],[(11,[],[8,9,10,11,1,2,3,4,5,6,7],[],[])]]" | ./pathManipulation -o ./Lyx/Pictures/Generated/${nameStock}.svg -w 500

result=$"\n9\n10\n11\n1\n2\n3\n4\n5\n6\n7\n8"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt



# shuffelMutate
nameStock=$"shuffelMutate"

echo "[[(11,[3,9],[1,2,3,4,5,6,7,8,9,10,11],[],[])],[(11,[3,9],[1,2,3,6,8,5,7,4,9,10,11],[],[])]]" | ./pathManipulation -o ./Lyx/Pictures/Generated/${nameStock}.svg -w 500

result=$"1\n2\n3\n6\n8\n5\n7\n4\n9\n10\n11"
echo -e $result > tmp.txt
gnuplot -e "dataName ='tmp.txt'; pdfName='Lyx/Pictures/Generated/${nameStock}Hist.pdf'" plotOrder.plt





rm tmp.txt
