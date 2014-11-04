ghc --make pathManipulation.lhs
# chmod +x pa
echo "(9, [[1,2,3,4,5,6,7,8,9]], [], [[1,2,7,4,5,6,3,8,9]], [3,7], [])" | ./pathManipulation -o exampleMutation1.svg -h 500
echo "(9, [[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]], [], [[1,3,2,4,5,6,7,8,9]], [3,7], [])" | ./pathManipulation -o exampleMutation2.svg -h 500
echo "(9, [[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]], [], [[1,3,2,4,5,6,7,8,9]], [3,7], [])" | ./pathManipulation -o exampleMutation3.svg -h 500
echo "(9, [[1,2,3,4,5,6,7,8,9],[1,2,4,3,5,6,7,8,9],[1,3,2,4,5,6,7,8,9],[1,3,4,2,5,6,7,8,9],[1,4,2,3,5,6,7,8,9],[1,4,3,2,5,6,7,8,9]], [], [[1,3,2,4,5,6,7,8,9]], [3,7], [])" | ./pathManipulation -o exampleMutation4.svg -h 500
echo "done"
