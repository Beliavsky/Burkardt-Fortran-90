#! /bin/bash
#
gfortran -c -Wall asa245_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa245_test asa245_test.o $HOME/lib/asa245.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa245_test.o
#
./asa245_test > asa245_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa245_test
#
echo "Normal end of execution."
