#! /bin/bash
#
gfortran -c -Wall asa239_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa239_test asa239_test.o $HOME/lib/asa239.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa239_test.o
#
./asa239_test > asa239_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa239_test
#
echo "Normal end of execution."
