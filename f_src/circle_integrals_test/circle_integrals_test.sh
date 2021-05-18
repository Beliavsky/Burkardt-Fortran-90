#! /bin/bash
#
gfortran -c -Wall circle_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o circle_integrals_test circle_integrals_test.o $HOME/lib/circle_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm circle_integrals_test.o
#
./circle_integrals_test > circle_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm circle_integrals_test
#
echo "Normal end of execution."
