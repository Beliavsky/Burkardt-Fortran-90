#! /bin/bash
#
gfortran -c -Wall disk01_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o disk01_integrals_test disk01_integrals_test.o $HOME/lib/disk01_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk01_integrals_test.o
#
./disk01_integrals_test > disk01_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk01_integrals_test
#
echo "Normal end of execution."
