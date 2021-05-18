#! /bin/bash
#
gfortran -c -Wall asa299_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa299_test asa299_test.o $HOME/lib/asa299.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa299_test.o
#
./asa299_test > asa299_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa299_test
#
echo "Normal end of execution."
