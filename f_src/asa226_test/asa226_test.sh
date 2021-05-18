#! /bin/bash
#
gfortran -c -Wall asa226_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa226_test asa226_test.o $HOME/lib/asa226.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa226_test.o
#
./asa226_test > asa226_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa226_test
#
echo "Normal end of execution."
