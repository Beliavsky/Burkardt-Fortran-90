#! /bin/bash
#
gfortran -c -Wall latinize_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran latinize_test.o $HOME/lib/latinize.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm latinize_test.o
#
mv a.out latinize_test
./latinize_test > latinize_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm latinize_test
#
echo "Normal end of execution."
