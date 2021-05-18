#! /bin/bash
#
gfortran -c -Wall asa053_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa053_test asa053_test.o /$HOME/lib/asa053.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa053_test.o
#
./asa053_test > asa053_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa053_test
#
echo "Normal end of execution."
