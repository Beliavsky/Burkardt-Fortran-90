#! /bin/bash
#
gfortran -c -Wall asa159_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa159_test asa159_test.o $HOME/lib/asa159.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa159_test.o
#
./asa159_test > asa159_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa159_test
#
echo "Normal end of execution."
