#! /bin/bash
#
gfortran -c -Wall asa310_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa310_test asa310_test.o $HOME/lib/asa310.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa310_test.o
#
./asa310_test > asa310_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa310_test
#
echo "Normal end of execution."
