ghc --make pathManipulation.lhs
# chmod +x pa
echo "[[(9,[1,2,3,4,5,6,7,8,9],[],[3,7]), (9,[1,2,3,4,5,6,7,8,9],[],[3,7]), (9,[1,2,3,4,5,6,7,8,9],[],[3,7])], [(9,[1,2,3,4,5,6,7,8,9],[],[3,7]), (9,[1,2,3,4,5,6,7,8,9],[],[3,7]), (9,[1,2,3,4,5,6,7,8,9],[],[3,7])], [(9,[1,2,3,4,5,6,7,8,9],[],[3,7]), (9,[1,2,3,4,5,6,7,8,9],[],[3,7]), (9,[1,2,3,4,5,6,7,8,9],[],[3,7])]]" | ./pathManipulation -o exampleMutation1.svg -h 500

