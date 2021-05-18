#! /bin/bash
#
gfortran -c -Wall asa243_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa243_test asa243_test.o $HOME/lib/asa243.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa243_test.o
#
./asa243_test > asa243_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa243_test
#
echo "Normal end of execution."
