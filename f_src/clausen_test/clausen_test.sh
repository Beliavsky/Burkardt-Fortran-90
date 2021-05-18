#! /bin/bash
#
gfortran -c -Wall clausen_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o clausen_test clausen_test.o $HOME/lib/clausen.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm clausen_test.o
#
./clausen_test > clausen_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm clausen_test
#
echo "Normal end of execution."
