#! /bin/bash
#
gfortran -c -Wall bins_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bins_test bins_test.o $HOME/lib/bins.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bins_test.o
#
./bins_test > bins_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bins_test
#
echo "Normal end of execution."
