#! /bin/bash
#
gfortran -c -Wall asa241_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa241_test asa241_test.o $HOME/lib/asa241.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa241_test.o
#
./asa241_test > asa241_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa241_test
#
echo "Normal end of execution."
