#! /bin/bash
#
gfortran -c -Wall asa006_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa006_test asa006_test.o /$HOME/lib/asa006.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa006_test.o
#
./asa006_test > asa006_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa006_test
#
echo "Normal end of execution."
