#! /bin/bash
#
gfortran -c -Wall binary_tree.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran binary_tree.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm binary_tree.o
rm *.mod
#
mv a.out binary_tree
./binary_tree > binary_tree.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm binary_tree
#
echo "Normal end of execution."
