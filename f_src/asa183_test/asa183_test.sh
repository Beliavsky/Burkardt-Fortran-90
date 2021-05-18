#! /bin/bash
#
gfortran -c -Wall asa183_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa183_test asa183_test.o $HOME/lib/asa183.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa183_test.o
#
./asa183_test > asa183_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa183_test
#
echo "Normal end of execution."
