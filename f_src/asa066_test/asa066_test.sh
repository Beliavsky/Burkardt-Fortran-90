#! /bin/bash
#
gfortran -c -Wall asa066_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa066_test asa066_test.o /$HOME/lib/asa066.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa066_test.o
#
./asa066_test > asa066_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa066_test
#
echo "Normal end of execution."
