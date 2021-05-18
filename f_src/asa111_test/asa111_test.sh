#! /bin/bash
#
gfortran -c -Wall asa111_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa111_test asa111_test.o $HOME/lib/asa111.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa111_test.o
#
./asa111_test > asa111_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa111_test
#
echo "Normal end of execution."
