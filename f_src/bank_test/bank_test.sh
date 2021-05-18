#! /bin/bash
#
gfortran -c -Wall bank_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bank_test bank_test.o $HOME/lib/bank.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bank_test.o
#
./bank_test > bank_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bank_test
#
echo "Normal end of execution."
