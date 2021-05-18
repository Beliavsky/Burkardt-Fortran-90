#! /bin/bash
#
gfortran -c -Wall randlc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran randlc_test.o $HOME/lib/randlc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm randlc_test.o
#
mv a.out randlc_test
./randlc_test > randlc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm randlc_test
#
echo "Normal end of execution."
