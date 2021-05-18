#! /bin/bash
#
gfortran -c -Wall continued_fraction_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o continued_fraction_test continued_fraction_test.o $HOME/lib/continued_fraction.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm continued_fraction_test.o
#
./continued_fraction_test > continued_fraction_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm continued_fraction_test
#
echo "Normal end of execution."
