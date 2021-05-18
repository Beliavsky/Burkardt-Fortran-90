#! /bin/bash
#
gfortran -c -Wall asa091_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa091_test asa091_test.o /$HOME/lib/asa091.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa091_test.o
#
./asa091_test > asa091_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa091_test
#
echo "Normal end of execution."
