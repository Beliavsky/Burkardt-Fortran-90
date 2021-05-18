#! /bin/bash
#
gfortran -c -Wall asa109_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa109_test asa109_test.o $HOME/lib/asa109.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa109_test.o
#
./asa109_test > asa109_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa109_test
#
echo "Normal end of execution."
