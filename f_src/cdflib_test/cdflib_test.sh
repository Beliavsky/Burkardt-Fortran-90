#! /bin/bash
#
gfortran -c -Wall cdflib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cdflib_test cdflib_test.o $HOME/lib/cdflib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cdflib_test.o
#
./cdflib_test > cdflib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cdflib_test
#
echo "Normal end of execution."
